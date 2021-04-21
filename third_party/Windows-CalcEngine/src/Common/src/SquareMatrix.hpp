#pragma once

#include <vector>

namespace FenestrationCommon
{
    // Works only with double
    class SquareMatrix
    {
    public:
        explicit SquareMatrix(const std::size_t tSize = 0);
        explicit SquareMatrix(const std::initializer_list<std::vector<double>> & tInput);
        explicit SquareMatrix(const std::vector<std::vector<double>> & tInput);
        explicit SquareMatrix(const std::vector<std::vector<double>> && tInput);

        std::size_t size() const;
        void setZeros();
        void setIdentity();
        void setDiagonal(const std::vector<double> & tInput);

        std::vector<size_t> makeUpperTriangular();

        SquareMatrix inverse() const;

        double operator()(const std::size_t i, const std::size_t j) const;
        double & operator()(const std::size_t i, const std::size_t j);

        SquareMatrix mmultRows(const std::vector<double> & tInput);

		std::vector<std::vector<double>> getMatrix() const;

    private:
        // explicit SquareMatrix(SquareMatrix && tMatrix);
        SquareMatrix LU() const;
        std::vector<double> checkSingularity() const;
        std::size_t m_size;
        std::vector<std::vector<double>> m_Matrix;
    };

    SquareMatrix operator*(const SquareMatrix & first, const SquareMatrix & second);
    SquareMatrix operator*=(SquareMatrix & first, const SquareMatrix & second);
    SquareMatrix operator+(const SquareMatrix & first, const SquareMatrix & second);
    SquareMatrix operator+=(SquareMatrix & first, const SquareMatrix & second);
    SquareMatrix operator-(const SquareMatrix & first, const SquareMatrix & second);
    SquareMatrix operator-=(SquareMatrix & first, const SquareMatrix & second);

    std::vector<double> operator*(const std::vector<double> & first, const SquareMatrix & second);
    std::vector<double> operator*(const SquareMatrix & first, const std::vector<double> & second);

}   // namespace FenestrationCommon
