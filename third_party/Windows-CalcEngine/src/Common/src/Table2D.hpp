#pragma once

#include <vector>
#include <stdexcept>

namespace Table
{
    template<class T>
    class Table2D
    {
    public:
        explicit Table2D(std::vector<T> x, std::vector<T> y, std::vector<std::vector<T>> values);

        T operator()(size_t i, size_t j) const;
        [[nodiscard]] T x_value(size_t i) const;
        [[nodiscard]] std::vector<T> x_values() const;
        [[nodiscard]] T y_value(size_t i) const;
        [[nodiscard]] std::vector<T> y_values() const;
        [[nodiscard]] std::vector<T> row(size_t i) const;
        [[nodiscard]] std::vector<T> column(size_t i) const;

    private:
        std::vector<T> x_;
        std::vector<T> y_;
        std::vector<std::vector<T>> values_;
    };

    template<class T>
    Table2D<T>::Table2D(std::vector<T> x, std::vector<T> y, std::vector<std::vector<T>> values) :
        x_(std::move(x)), y_(std::move(y)), values_(std::move(values))
    {
        if(x_.size() != values_.size())
        {
            throw std::runtime_error("Header size does not match provided table size.");
        }
    }

    template<class T>
    T Table2D<T>::operator()(size_t i, size_t j) const
    {
        return values_[i][j];
    }

    template<class T>
    T Table2D<T>::x_value(size_t i) const
    {
        return x_[i];
    }

    template<class T>
    T Table2D<T>::y_value(size_t i) const
    {
        return y_[i];
    }

    template<class T>
    std::vector<T> Table2D<T>::row(size_t i) const
    {
        return values_[i];
    }

    template<class T>
    std::vector<T> Table2D<T>::column(size_t i) const
    {
        std::vector<T> result;
        for(const auto & vec: values_)
        {
            result.push_back(vec[i]);
        }

        return result;
    }

    template<class T>
    std::vector<T> Table2D<T>::x_values() const
    {
        return x_;
    }

    template<class T>
    std::vector<T> Table2D<T>::y_values() const
    {
        return y_;
    }
}   // namespace Table
