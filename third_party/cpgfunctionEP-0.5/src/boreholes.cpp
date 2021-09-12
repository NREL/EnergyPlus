//
// Created by jackcook on 7/11/20.
//

#include <cpgfunction/boreholes.h>
#include <algorithm>

using namespace std;

namespace gt {

    double Distance_Formula(double x1, double y1, double x2, double y2) {
        return sqrt(pow((x1 - x2), 2) + pow((y1 - y2), 2));
    }
    
}  // namespace gt

namespace gt::boreholes {

    double Borehole::distance(Borehole target) {
        double x1 = x;
        double y1 = y;
        double x2 = target.x;
        double y2 = target.y;
        double dist = Distance_Formula(x1, y1, x2, y2);
        return max(r_b, double(dist));  // max needs doubles
    }

    tuple<double, double> Borehole::position() {
        tuple<double, double> t (x, y);
        return t;
    };

    vector<Borehole> boreField(const vector<tuple<double, double>> &coordinates,
                               const double &r_b, const double &H,
                               const double &D){
        vector<Borehole> bores(coordinates.size());

        double x;
        double y;

        for (size_t i = 0; i < coordinates.size(); i++) {
            x = get<0>(coordinates[i]);
            y = get<1>(coordinates[i]);
            bores[i] = Borehole(H, D, r_b, x, y);
        }  // next i

        return bores;
    }  // boreField();

    void Similarity::similarities(SimilaritiesType &SimReal,
                                  SimilaritiesType &SimImage,
                                  vector<gt::boreholes::Borehole> &boreSegments,
                                  bool splitRealAndImage, double disTol,
                                  double tol) {
        // TODO: fork a pool

        // declare the variables local to this function
        int nDis;
        vector<double> disPairs;
        vector<int> nPairs;
        vector< vector < tuple <int, int> > > Pairs;
        _similarities_group_by_distance(boreSegments, Pairs, nPairs,
                                        disPairs, nDis);

        vector<SimilaritiesType> RealSimT(Pairs.size());
        vector<SimilaritiesType> ImageSimT;

        // if real and image parts of the FLS are split,
        // evaluate real and image similarities seperately:
        if (splitRealAndImage) {
            ImageSimT.resize(Pairs.size());
            for (size_t i=0; i<Pairs.size(); i++) {
                // TODO: thread both of these
                _similarities_one_distance(RealSimT[i],Pairs[i],
                                           boreSegments, "real");
                _similarities_one_distance(ImageSimT[i],Pairs[i],
                                           boreSegments, "image");
            } // next i
            int a = 1;
        } else {
            throw invalid_argument("splitRealAndImage == false code not "
                                   "implemented yet.");
        } //

        // TODO: close pool
        // aggregate all real similarities for all distances

        auto _aggregate = [&nDis, &disPairs](vector<SimilaritiesType> &SimFrom,
                SimilaritiesType &SimTo) {
            int nSimtmp;
            // reserve and insert
            auto _res_and_ins = [](vector<SimilaritiesType> &SimFrom,
                    SimilaritiesType &SimTo, const int &i) {
                // Sim positions
                SimTo.Sim.reserve(SimTo.Sim.size() +
                    distance(SimFrom[i].Sim.begin(), SimFrom[i].Sim.end()));
                SimTo.Sim.insert(SimTo.Sim.end(), SimFrom[i].Sim.begin(),
                                 SimFrom[i].Sim.end());
                // H values
                SimTo.HSim.reserve(SimTo.HSim.size() +
                    distance(SimFrom[i].HSim.begin(), SimFrom[i].HSim.end()));
                SimTo.HSim.insert(SimTo.HSim.end(), SimFrom[i].HSim.begin(),
                                  SimFrom[i].HSim.end());
                // D Values
                SimTo.DSim.reserve(SimTo.DSim.size() +
                    distance(SimFrom[i].DSim.begin(), SimFrom[i].DSim.end()));
                SimTo.DSim.insert(SimTo.DSim.end(), SimFrom[i].DSim.begin(),
                                  SimFrom[i].DSim.end());
            };
            for (int i=0; i<nDis; i++) {
                nSimtmp = SimFrom[i].nSim;
                SimTo.nSim += SimFrom[i].nSim;
                for (int j = 0; j < nSimtmp; j++) {
                    SimTo.disSim.push_back(disPairs[i]);
                }  // next j
                vector<vector<tuple<int, int> > > tmp;
                tmp = SimFrom[i].Sim;
                _res_and_ins(SimFrom, SimTo, i);
            }  // next i
        };  // auto _aggregate();
        // Sim positions
        _aggregate(RealSimT, SimReal);
        if (splitRealAndImage) {
            _aggregate(ImageSimT, SimImage);
        } // fi
    } // Similarity::similarities

    void Similarity::_similarities_group_by_distance(
            vector<gt::boreholes::Borehole> &boreSegments,
            vector< vector < tuple <int, int> > > &Pairs, vector<int> &nPairs,
            vector<double> &disPairs, int &nDis, double disTol) {
        // initialize lists
        nPairs.push_back(1);
        vector< tuple <int, int > > vect_w_tup(1);
        vect_w_tup[0] = tuple<int, int> (0, 0);
        Pairs.push_back(vect_w_tup);
        disPairs.push_back(boreSegments[0].r_b);
        nDis = 1;

        int nb = boreSegments.size();
        gt::boreholes::Borehole b1;
        gt::boreholes::Borehole b2;
        int i2;
        double dis;
        double rTol;
        double diff;
        for (int i=0; i<nb; i++) {
            b1 = boreSegments[i];
            if (i == 0) {
                i2 = i + 1;
            } else {
                i2 = i;
            } // fi i == 0
            for (int j = i2; j < nb; j++) {
                b2 = boreSegments[j];
                // distance between current pairs of boreholes
                dis = b1.distance(b2);
                if (i == j) {
                    // the relative tolerance is ued for same-borehole distances
                    rTol = 1.0e-6 * b1.r_b;
                } else {
                    rTol = disTol;
                } // fi i == j
                // verify if the current pair should be included in the
                // previously identified similarities
                for (int k=0; k<nDis; k++) {
                    diff = abs(disPairs[k] - dis);
                    if (diff < rTol) {
                        Pairs[k].push_back(tuple<int, int> (i, j));
                        nPairs[k]++;
                        break;
                    } // fi disPairs[k] - dis < rTol
                    // add symmetry to the list if no match was found
                    if (k == nDis-1) {
                        nDis++;
                        disPairs.push_back(dis);
                        vect_w_tup[0] = tuple<int, int> (i, j);
                        Pairs.push_back(vect_w_tup);
                        nPairs.push_back(1);
                        break;
                    }
                } // next k
            } // for j in range(i2, nb)
        } // for i in range(nb)
        int a = 1;
    } // Similarity::_similarities_group_by_distance

    void Similarity::_similarities_one_distance(
            SimilaritiesType & SimT, vector<tuple<int, int> > &pairs,
            vector<gt::boreholes::Borehole> &boreSegments, const string& kind,
            double tol) {
        // Condition for equivalence of the real part of the FLS solution
        auto compare_real_segments = [](const double &H1a, const double &H1b,
                const double &H2a, const double &H2b, const double &D1a,
                const double &D1b, const double &D2a, const double &D2b,
                const double &tol){
            bool similarity;
            similarity = abs((H1a - H1b) / H1a) < tol &&
                    abs((H2a - H2b) / H2a) < tol &&
                    abs(((D2a - D1a) - (D2b - D1b)) / (D2a - D1a + 1e-30)) < tol;
            return similarity;
        };
        // Condition for equivalence of the image part of the FLS solution
        auto compare_image_segments = [](const double &H1a, const double &H1b,
                const double &H2a, const double &H2b, const double &D1a,
                const double &D1b, const double &D2a, const double &D2b,
                const double &tol) {
            bool similarity;
            similarity = abs((H1a - H1b) / H1a) < tol &&
                    abs((H2a - H2b) / H2a) < tol &&
                    abs(((D2a + D1a) - (D2b + D1b)) / (D2a + D1a + 1e-30)) < tol;
            return similarity;
        };
        // Condition for equivalence of the full FLS solution
        auto compare_realandimage_segments = [](const double &H1a,
                const double &H1b, const double &H2a, const double &H2b,
                const double &D1a, const double &D1b, const double &D2a,
                const double &D2b, const double &tol) {
            bool similarity;
            similarity = abs((H1a - H1b) / H1a) < tol &&
                    abs((H2a - H2b) / H2a) < tol &&
                    abs((D1a - D1b) / (D1a + 1e-30)) < tol &&
                    abs((D2a - D2b) / (D2a + 1e-30)) < tol;
            return similarity;
        };

        string real("real");
        string image("image");
        string realandimage("realandimage");

        // compare segments is a pointer to one of the lambda functions
        bool(*compare_segments)(const double&, const double&, const double&,
                const double&, const double&, const double&, const double&,
                const double&, const double&);

        if (real.compare(kind) == 0) {
            compare_segments = compare_real_segments;
        } else if (image.compare(kind) == 0) {
            compare_segments = compare_image_segments;
        } else if (realandimage.compare(kind) == 0) {
            compare_segments = compare_realandimage_segments;
        } else {
            throw invalid_argument("Error kind not implemented yet.");
        }

        SimT.nSim = 1;
        tuple<double, double> doub_tup_temp_H;
        tuple<double, double> doub_tup_temp_D;
        tuple<int, int> int_tup_temp_sim;
        vector< tuple <int, int > > vect_w_tup(1);
        vect_w_tup[0] = pairs[0];
//        tuple<int, int> pair0 = pairs[0];
        int i0 = get<0>(pairs[0]);
        int j0 = get<1>(pairs[0]);
        SimT.Sim.push_back(vect_w_tup);
        doub_tup_temp_H = make_tuple(boreSegments[i0].H, boreSegments[j0].H);
        SimT.HSim.push_back(doub_tup_temp_H);
        doub_tup_temp_D = make_tuple(boreSegments[i0].D, boreSegments[j0].D);
        SimT.DSim.push_back(doub_tup_temp_D);

        // values used in loops
        int ibor;
        int jbor;
        gt::boreholes::Borehole b1;
        gt::boreholes::Borehole b2;
        double H1;
        double H2;
        double D1;
        double D2;

        // Cycle through all pairs of boreholes for the given distance
        for (size_t i=1; i<pairs.size(); i++) {
            ibor = get<0>(pairs[i]);
            jbor = get<1>(pairs[i]);
            if (ibor > jbor) {
                swap(ibor, jbor);
            }
            b1 = boreSegments[ibor];
            b2 = boreSegments[jbor];
            // Verify if the current pair should be included in the previously identified symmetries
            for (int j=0; j<SimT.nSim; j++) {
                H1 = get<0>(SimT.HSim[j]);
                H2 = get<1>(SimT.HSim[j]);
                D1 = get<0>(SimT.DSim[j]);
                D2 = get<1>(SimT.DSim[j]);
                if (compare_segments(H1, b1.H, H2, b2.H, D1, b1.D, D2, b2.D,
                                     tol)) {
                    int_tup_temp_sim = make_tuple(ibor, jbor);
                    SimT.Sim[j].push_back(int_tup_temp_sim);
                    break;
                } else if (compare_segments(H1, b2.H, H2, b1.H, D1, b2.D, D2,
                                            b1.D, tol)) {
                    int_tup_temp_sim = make_tuple(jbor, ibor);
                    SimT.Sim[j].push_back(int_tup_temp_sim);
                    break;
                } else if (j == SimT.nSim-1) {
                    SimT.nSim++;
                    int_tup_temp_sim = make_tuple(ibor, jbor);
                    vect_w_tup[0] = int_tup_temp_sim;
                    SimT.Sim.push_back(vect_w_tup);
                    doub_tup_temp_H = make_tuple(b1.H, b2.H);
                    SimT.HSim.push_back(doub_tup_temp_H);
                    doub_tup_temp_D = make_tuple(b1.D, b2.D);
                    SimT.DSim.push_back(doub_tup_temp_D);
                    break;
                }
            } // next j
        } // next i
    }  // Similarity::_similarities_one_distance

} // namespace gt::boreholes
