#include <stdexcept>
#include <cassert>

#include "LinearSolver.hpp"
#include "SquareMatrix.hpp"

#include <cmath>


namespace FenestrationCommon
{

    std::vector<double> CLinearSolver::solveSystem(SquareMatrix & t_MatrixA, std::vector<double> & t_VectorB) {
        if(t_MatrixA.size() != t_VectorB.size())
        {
            throw std::runtime_error("Matrix and vector for system of linear equations are not same size.");
        }

        std::vector<size_t> index = t_MatrixA.makeUpperTriangular();

        const int size = int(t_MatrixA.size());

        int ii = -1;
        for(int i = 0; i < size; ++i)
        {
            size_t ll = index[i];
            double sum = t_VectorB[ll];
            t_VectorB[ll] = t_VectorB[i];
            if(ii != -1)
            {
                for(int j = ii; j <= i - 1; ++j)
                {
                    sum -= t_MatrixA(i, j) * t_VectorB[j];
                }   // j
            }
            else if(sum != 0.0)
            {
                ii = int(i);
            }
            t_VectorB[i] = sum;
        }   // i

        for(int i = (size - 1); i >= 0; --i)
        {
            double sum = t_VectorB[i];
            for(int j = i + 1; j < size; ++j)
            {
                sum -= t_MatrixA(i, j) * t_VectorB[j];
            }   // j
            t_VectorB[i] = sum / t_MatrixA(i, i);
        }   // i

        return t_VectorB;
    }

}   // namespace FenestrationCommon
