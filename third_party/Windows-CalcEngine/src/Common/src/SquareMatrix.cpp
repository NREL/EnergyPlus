#include <cassert>
#include <stdexcept>
#include <thread>

#include "SquareMatrix.hpp"



namespace FenestrationCommon {
	CSquareMatrix::CSquareMatrix( const size_t aSize ) {
		m_Size = aSize;
		m_Matrix.resize( m_Size );
		for ( size_t i = 0; i < m_Size; ++i )
			m_Matrix[ i ].resize( m_Size );
	}

	size_t CSquareMatrix::getSize() const {
		return m_Size;
	}

	void CSquareMatrix::setZeros() {
		for ( size_t i = 0; i < m_Size; ++i ) {
			for ( size_t j = 0; j < m_Size; ++j ) {
				m_Matrix[ i ][ j ] = 0;
			}
		}
	}

	void CSquareMatrix::setIdentity() {
		setZeros();
		for ( size_t i = 0; i < m_Size; ++i ) {
			m_Matrix[ i ][ i ] = 1;
		}
	}

	void CSquareMatrix::setDiagonal( std::vector< double > const& t_Values ) {
		if ( t_Values.size() != m_Size ) {
			throw std::runtime_error( "Supplied vector size mismatch matrix size" );
		}
		setZeros();
		for ( size_t i = 0; i < m_Size; ++i ) {
			m_Matrix[ i ][ i ] = t_Values[ i ];
		}
	}

	std::vector< double >& CSquareMatrix::operator[]( size_t const index ) {
		assert( index < m_Size );
		return m_Matrix[ index ];
	}

	std::vector< double > const& CSquareMatrix::operator[]( size_t const index ) const {
		assert( index < m_Size );
		return m_Matrix[ index ];
	}

	std::shared_ptr< CSquareMatrix > CSquareMatrix::add( CSquareMatrix const& t_Matrix ) const {
		if ( m_Size != t_Matrix.m_Size ) {
			throw std::runtime_error( "Matrices must be identical in size." );
		}

		auto aMatrix = std::make_shared< CSquareMatrix >( m_Size );
		for ( size_t i = 0; i < m_Size; ++i ) {
			for ( size_t j = 0; j < t_Matrix.m_Size; ++j ) {
				( *aMatrix )[ i ][ j ] = m_Matrix[ i ][ j ] + t_Matrix.m_Matrix[ i ][ j ];
			}
		}

		return aMatrix;
	}

	std::shared_ptr< CSquareMatrix > CSquareMatrix::sub( CSquareMatrix const& t_Matrix ) const {
		if ( m_Size != t_Matrix.m_Size ) {
			throw std::runtime_error( "Matrices must be identical in size." );
		}

		auto aMatrix = std::make_shared< CSquareMatrix >( m_Size );
		for ( size_t i = 0; i < m_Size; ++i ) {
			for ( size_t j = 0; j < t_Matrix.m_Size; ++j ) {
				( *aMatrix )[ i ][ j ] = m_Matrix[ i ][ j ] - t_Matrix.m_Matrix[ i ][ j ];
			}
		}

		return aMatrix;
	}

	std::shared_ptr< CSquareMatrix > CSquareMatrix::mult( CSquareMatrix const& t_Matrix ) const {
		if ( m_Size != t_Matrix.m_Size ) {
			throw std::runtime_error( "Matrices must be identical in size." );
		}

		auto aMatrix = std::make_shared< CSquareMatrix >( m_Size );

		for ( size_t i = 0; i < m_Size; ++i ) {
			for ( size_t k = 0; k < m_Size; ++k ) {
				for ( size_t j = 0; j < t_Matrix.m_Size; ++j ) {
					( *aMatrix )[ i ][ j ] += m_Matrix[ i ][ k ] * t_Matrix.m_Matrix[ k ][ j ];
					// ( *aMatrix )[ i ][ j ] = ( *aMatrix )[ i ][ j ] + m_Matrix[ i ][ k ] * t_Matrix.m_Matrix[ k ][ j ];
				}
			}
		}

		return aMatrix;

	}

	std::shared_ptr< std::vector< double > > CSquareMatrix::multMxV( std::vector< double > const& t_Vector ) const {
		if ( m_Size != t_Vector.size() ) {
			throw std::runtime_error( "Matrix and vector does not have same number of rows and columns."
			                    " It is not possible to perform multiplication." );
		}

		auto aResult = std::make_shared< std::vector< double > >( m_Size );

		for ( size_t i = 0; i < m_Size; ++i ) {
			for ( size_t j = 0; j < m_Size; ++j ) {
				( *aResult )[ i ] = ( *aResult )[ i ] + m_Matrix[ i ][ j ] * t_Vector[ j ];
			}
		}

		return aResult;
	}

	std::shared_ptr< std::vector< double > > CSquareMatrix::multVxM( std::vector< double > const& t_Vector ) const {
		if ( m_Size != t_Vector.size() ) {
			throw std::runtime_error( "Matrix and vector do not have same number of rows and columns."
			                    " It is not possible to perform multiplication." );
		}

		auto aResult = std::make_shared< std::vector< double > >( m_Size );

		for ( size_t i = 0; i < m_Size; ++i ) {
			for ( size_t j = 0; j < m_Size; ++j ) {
				( *aResult )[ i ] = ( *aResult )[ i ] + m_Matrix[ j ][ i ] * t_Vector[ j ];
			}
		}

		return aResult;
	}

	void CSquareMatrix::copyFrom( CSquareMatrix const& t_Matrix ) {
		if ( m_Size != t_Matrix.m_Size ) {
			throw std::runtime_error( "Matrices must be identical in size" );
		}
		for ( size_t i = 0; i < m_Size; ++i ) {
			m_Matrix[ i ] = t_Matrix.m_Matrix[ i ];
		}
	}

	std::shared_ptr< CSquareMatrix > CSquareMatrix::inverse() {
		// return LU decomposed matrix of current matrix
		auto aLU = LU();

		// find the inverse
		auto inverse = std::make_shared< CSquareMatrix >( m_Size );
		std::vector< double > d( m_Size );
		std::vector< double > y( m_Size );

		auto size = int( m_Size - 1 );

		for ( auto m = 0; m <= size; ++m ) {
			fill( d.begin(), d.end(), 0 );
			fill( y.begin(), y.end(), 0 );
			d[ m ] = 1;
			for ( auto i = 0; i <= size; ++i ) {
				double x = 0;
				for ( auto j = 0; j <= i - 1; ++j ) {
					x = x + ( *aLU )[ i ][ j ] * y[ j ];
				}
				y[ i ] = ( d[ i ] - x );
			}

			for ( auto i = size; i >= 0; --i ) {
				auto x = 0.0;
				for ( auto j = i + 1; j <= size; ++j ) {
					x = x + ( *aLU )[ i ][ j ] * ( *inverse )[ j ][ m ];
				}
				( *inverse )[ i ][ m ] = ( y[ i ] - x ) / ( *aLU )[ i ][ i ];
			}
		}

		return inverse;
	}

	std::shared_ptr< CSquareMatrix > CSquareMatrix::LU() {
		auto D = std::make_shared< CSquareMatrix >( m_Size );
		D->copyFrom( *this );

		for ( auto k = 0u; k <= m_Size - 2; ++k ) {
			for ( auto j = k + 1; j <= m_Size - 1; ++j ) {
				auto x = ( *D )[ j ][ k ] / ( *D )[ k ][ k ];
				for ( auto i = k; i <= m_Size - 1; ++i ) {
					( *D )[ j ][ i ] = ( *D )[ j ][ i ] - x * ( *D )[ k ][ i ];
				}
				( *D )[ j ][ k ] = x;
			}
		}

		return D;
	}

}
