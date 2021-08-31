//
// Created by jackcook on 8/17/21.
//

#ifndef LU_DECOMPOSITION_LU_H
#define LU_DECOMPOSITION_LU_H

#include <vector>

namespace jcc {
    void decomposition(std::vector<std::vector<double> > &A, int &n, std::vector<int> &indx,
                       double &d);

    void back_substitution(std::vector<std::vector<double> > &A, int &n, std::vector<int> indx,
                           std::vector<double> &b);
}

#endif //LU_DECOMPOSITION_LU_H
