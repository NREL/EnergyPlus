#ifndef LINEARSOLVER_H
#define LINEARSOLVER_H

#include <vector>

namespace FenestrationCommon
{
    class SquareMatrix;

    class CLinearSolver
    {
    public:

        static std::vector<double> solveSystem(SquareMatrix & t_MatrixA, std::vector<double> & t_VectorB);

    private:
        
    };
}   // namespace FenestrationCommon

#endif
