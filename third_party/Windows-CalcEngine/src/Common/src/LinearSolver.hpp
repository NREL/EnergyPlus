#ifndef LINEARSOLVER_H
#define LINEARSOLVER_H

#include <vector>

namespace FenestrationCommon {

	class CSquareMatrix;

	class CLinearSolver {
	public:
		CLinearSolver();

		std::vector< double > solveSystem(
			CSquareMatrix& t_MatrixA,
			std::vector< double >& t_VectorB ) const;

	private:
		std::vector< double > checkSingularity( CSquareMatrix& t_MatrixA ) const;
		std::vector< size_t > makeUpperTriangular( CSquareMatrix& t_MatrixA ) const;

	};
}

#endif
