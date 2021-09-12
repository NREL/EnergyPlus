//
// Created by jackcook on 8/17/21.
//

#include <vector>
#include <cmath>
#include <stdexcept>
using namespace std;

#ifndef LU_DECOMPOSITION_LU_H
#define LU_DECOMPOSITION_LU_H

namespace jcc {
    void decomposition(vector<vector<double> > &A, int &n, vector<int> &indx,
                       double &d);

    void back_substitution(vector<vector<double> > &A, int &n, vector<int> indx,
                           vector<double> &b);
}

#endif //LU_DECOMPOSITION_LU_H
