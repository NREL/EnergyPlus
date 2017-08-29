#ifndef SQUAREMATRIXSERIES_H
#define SQUAREMATRIXSERIES_H

#include <vector>
#include <memory>

namespace FenestrationCommon {

	class CSeries;
	class CSquareMatrix;
	enum class IntegrationType;

	class CMatrixSeries {
	public:
		CMatrixSeries( const size_t t_Size1, const size_t t_Size2 );
		CMatrixSeries( CMatrixSeries const & t_MatrixSeries );

		// add property at specific series position
		void addProperty( const size_t i, const size_t j, const double t_Wavelength, const double t_Value );
		void addProperties( const size_t i, const double t_Wavelength,
		                    const std::vector< double >& t_Values );
		void addProperties( const double t_Wavelength, CSquareMatrix& t_Matrix );

		// Multiply all series in matrix with provided one
		void mMult( const CSeries& t_Series );

		// Multiplication of several series with matrix series
		void mMult( const std::vector< std::shared_ptr< CSeries > >& t_Series );

		std::vector< std::unique_ptr< CSeries > >& operator[]( const size_t index );

		void integrate( const IntegrationType t_Integration );

		std::shared_ptr< std::vector< std::shared_ptr< std::vector< double > > > >
		getSums( const double minLambda, const double maxLambda, const std::vector< double >& t_ScaleValue );

		std::shared_ptr< CSquareMatrix > getSquaredMatrixSums(
			const double minLambda, const double maxLambda, const std::vector< double >& t_ScaleValue );

		size_t size1() const;
		size_t size2() const;

	private:
		std::vector< std::vector< std::unique_ptr< CSeries > > > m_Matrix;
		size_t m_Size1;
		size_t m_Size2;
	};

}

#endif
