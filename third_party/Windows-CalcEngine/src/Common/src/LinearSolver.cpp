#include <stdexcept>
#include <cassert>

#include "LinearSolver.hpp"
#include "SquareMatrix.hpp"

#include <math.h>




namespace FenestrationCommon {

	CLinearSolver::CLinearSolver() {

	}

	std::vector< double > CLinearSolver::checkSingularity( CSquareMatrix& t_MatrixA ) const {
		size_t size = t_MatrixA.getSize();
		std::vector< double > vv;

		for ( auto i = 0u; i < size; ++i ) {
			auto aamax = 0.0;
			for ( size_t j = 0; j < size; ++j ) {
				double absCellValue = fabs( t_MatrixA[ i ][ j ] );
				if ( absCellValue > aamax ) {
					aamax = absCellValue;
				}
			}
			if ( aamax == 0 ) {
				assert( aamax != 0 );
			}
			vv.push_back( 1 / aamax );
		}

		return vv;
	}

	std::vector< size_t > CLinearSolver::makeUpperTriangular( CSquareMatrix& t_MatrixA ) const {
		auto TINY = 1e-20;

		int size = int( t_MatrixA.getSize() );
		std::vector< size_t > index( size );

		std::vector< double > vv = checkSingularity( t_MatrixA );

		auto d = 1;

		for ( auto j = 0; j < size; ++j ) {

			for ( auto i = 0; i <= j - 1; ++i ) {
				auto sum = t_MatrixA[ i ][ j ];
				for ( auto k = 0; k <= i - 1; ++k ) {
					sum = sum - t_MatrixA[ i ][ k ] * t_MatrixA[ k ][ j ];
				}
				t_MatrixA[ i ][ j ] = sum;
			}

			auto aamax = 0.0;
			auto imax = 0;

			for ( auto i = j; i < size; ++i ) {
				auto sum = t_MatrixA[ i ][ j ];
				for ( auto k = 0; k <= j - 1; ++k ) {
					sum = sum - t_MatrixA[ i ][ k ] * t_MatrixA[ k ][ j ];
				}
				t_MatrixA[ i ][ j ] = sum;
				auto dum = vv[ i ] * fabs( sum );
				if ( dum >= aamax ) {
					imax = i;
					aamax = dum;
				}
			}

			if ( j != imax ) {
				for ( auto k = 0; k < size; ++k ) {
					auto dum = t_MatrixA[ imax ][ k ];
					t_MatrixA[ imax ][ k ] = t_MatrixA[ j ][ k ];
					t_MatrixA[ j ][ k ] = dum;
				} // k
				d = -d;
				vv[ imax ] = vv[ j ];
			}
			index[ j ] = imax;
			if ( t_MatrixA[ j ][ j ] == 0.0 ) {
				t_MatrixA[ j ][ j ] = TINY;
			}
			if ( j != ( size - 1 ) ) {
				auto dum = 1.0 / t_MatrixA[ j ][ j ];
				for ( auto i = j + 1; i < size; ++i ) {
					t_MatrixA[ i ][ j ] = t_MatrixA[ i ][ j ] * dum;
				} // i
			}

		}

		return index;
	}

	std::vector< double > CLinearSolver::solveSystem(
		CSquareMatrix& t_MatrixA,
		std::vector< double >& t_VectorB ) const {

		if ( t_MatrixA.getSize() != t_VectorB.size() ) {
			std::runtime_error( "Matrix and vector for system of linear equations are not same size." );
		}

		std::vector< size_t > index = makeUpperTriangular( t_MatrixA );

		int size = int( t_MatrixA.getSize() );

		int ii = -1;
		for ( int i = 0; i < size; ++i ) {
			size_t ll = index[ i ];
			double sum = t_VectorB[ ll ];
			t_VectorB[ ll ] = t_VectorB[ i ];
			if ( ii != -1 ) {
				for ( int j = ii; j <= i - 1; ++j ) {
					sum -= t_MatrixA[ i ][ j ] * t_VectorB[ j ];
				} // j
			}
			else if ( sum != 0.0 ) {
				ii = int( i );
			}
			t_VectorB[ i ] = sum;
		} // i

		for ( int i = ( size - 1 ); i >= 0; --i ) {
			double sum = t_VectorB[ i ];
			for ( int j = i + 1; j < size; ++j ) {
				sum -= t_MatrixA[ i ][ j ] * t_VectorB[ j ];
			} // j
			t_VectorB[ i ] = sum / t_MatrixA[ i ][ i ];
		} // i

		return t_VectorB;

	}

}
