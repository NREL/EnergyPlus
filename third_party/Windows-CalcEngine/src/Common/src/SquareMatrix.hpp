#ifndef FENESTRATIONSQUAREMATRIX_H
#define FENESTRATIONSQUAREMATRIX_H

#include <vector>
#include <memory>

namespace FenestrationCommon {

	class CSquareMatrix {
	public:
		explicit CSquareMatrix( size_t const aSize );
		size_t getSize() const;
		void setZeros();
		// All diagonal items are one and all non diagonal are zero
		void setIdentity();
		// set diagonal values from vector
		void setDiagonal( std::vector< double > const& t_Values );
		std::vector< double >& operator[]( size_t const index );
		std::vector< double > const& operator[]( size_t const index ) const;
		//    double & operator[]( size_t i, size_t j );
		std::shared_ptr< CSquareMatrix > add( CSquareMatrix const& t_Matrix ) const;
		std::shared_ptr< CSquareMatrix > sub( CSquareMatrix const& t_Matrix ) const;
		std::shared_ptr< CSquareMatrix > mult( CSquareMatrix const& t_Matrix ) const;
		// Matrix multiplication with vector
		std::shared_ptr< std::vector< double > > multMxV( std::vector< double > const& t_Vector ) const;
		// Matrix multiplication with vector
		std::shared_ptr< std::vector< double > > multVxM( std::vector< double > const& t_Vector ) const;
		void copyFrom( CSquareMatrix const& t_Matrix );
		// inverse matrix
		std::shared_ptr< CSquareMatrix > inverse();

	private:
		// LU decomposition of current matrix
		std::shared_ptr< CSquareMatrix > LU();

		size_t m_Size;
		std::vector< std::vector< double > > m_Matrix;

	};
}


#endif
