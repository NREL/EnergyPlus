#include <stdexcept>
#include <algorithm>
#include <cmath>

#include "wceunique.hpp"
#include "Series.hpp"
#include "IntegratorStrategy.hpp"


namespace FenestrationCommon
{
    /////////////////////////////////////////////////////
    //  CSeriesPoint
    /////////////////////////////////////////////////////

    CSeriesPoint::CSeriesPoint() : m_x(0), m_Value(0)
    {}

    CSeriesPoint::CSeriesPoint(CSeriesPoint const & t_SeriesPoint)
    {
        *this = t_SeriesPoint;
    }

    CSeriesPoint::CSeriesPoint(double t_Wavelength, double t_Value) :
        m_x(t_Wavelength),
        m_Value(t_Value)
    {}

    std::unique_ptr<ISeriesPoint> CSeriesPoint::clone() const
    {
        return wce::make_unique<CSeriesPoint>(*this);
    }

    double CSeriesPoint::x() const
    {
        return m_x;
    }

    double CSeriesPoint::value() const
    {
        return m_Value;
    }

    void CSeriesPoint::value(double const t_Value)
    {
        m_Value = t_Value;
    }

    CSeriesPoint & CSeriesPoint::operator=(const CSeriesPoint & t_Property)
    {
        m_x = t_Property.m_x;
        m_Value = t_Property.m_Value;
        return *this;
    }

    bool CSeriesPoint::operator<(const CSeriesPoint & t_Point) const
    {
        return m_x < t_Point.m_x;
    }

    /////////////////////////////////////////////////////
    //  CSeries
    /////////////////////////////////////////////////////

    CSeries::CSeries(const std::vector<std::pair<double, double>> & t_values)
    {
        m_Series.clear();
        for(auto & val : t_values)
        {
            m_Series.push_back(wce::make_unique<CSeriesPoint>(val.first, val.second));
        }
    }

    CSeries::CSeries(const std::initializer_list<std::pair<double, double>> & t_values)
    {
        m_Series.clear();
        for(const auto & val : t_values)
        {
            m_Series.push_back(wce::make_unique<CSeriesPoint>(val.first, val.second));
        }
    }

    CSeries::CSeries(CSeries const & t_Series)
    {
        m_Series.clear();
        for(const auto & val : t_Series.m_Series)
        {
            m_Series.push_back(val->clone());
        }
    }

    void CSeries::addProperty(const double t_x, const double t_Value)
    {
        m_Series.push_back(wce::make_unique<CSeriesPoint>(t_x, t_Value));
    }

    void CSeries::insertToBeginning(double t_x, double t_Value)
    {
        m_Series.insert(m_Series.begin(), wce::make_unique<CSeriesPoint>(t_x, t_Value));
    }

    void CSeries::setConstantValues(const std::vector<double> & t_Wavelengths, double const t_Value)
    {
        m_Series.clear();
        for(auto it = t_Wavelengths.begin(); it < t_Wavelengths.end(); ++it)
        {
            addProperty((*it), t_Value);
        }
    }

    std::unique_ptr<CSeries> CSeries::integrate(IntegrationType t_IntegrationType,
                                                double normalizationCoefficient) const
    {
        // std::unique_ptr< CSeries > newProperties = wce::make_unique< CSeries >( );
        CIntegratorFactory aFactory = CIntegratorFactory();
        std::shared_ptr<IIntegratorStrategy> aIntegrator =
          aFactory.getIntegrator(t_IntegrationType);

        return aIntegrator->integrate(m_Series, normalizationCoefficient);
    }

    ISeriesPoint * CSeries::findLower(double const t_Wavelength) const
    {
        ISeriesPoint * currentProperty = nullptr;

        for(auto & spectralProperty : m_Series)
        {
            double aWavelength = spectralProperty->x();
            if(aWavelength > t_Wavelength)
            {
                break;
            }
            currentProperty = spectralProperty.get();
        }

        return currentProperty;
    }

    ISeriesPoint * CSeries::findUpper(double const t_Wavelength) const
    {
        ISeriesPoint * currentProperty = nullptr;

        for(auto & spectralProperty : m_Series)
        {
            double aWavelength = spectralProperty->x();
            if(aWavelength > t_Wavelength)
            {
                currentProperty = spectralProperty.get();
                break;
            }
        }

        return currentProperty;
    }

    double CSeries::interpolate(ISeriesPoint * t_Lower,
                                ISeriesPoint * t_Upper,
                                double const t_Wavelength)
    {
        double w1 = t_Lower->x();
        double w2 = t_Upper->x();
        double v1 = t_Lower->value();
        double v2 = t_Upper->value();
        double vx = 0;
        if(w2 != w1)
        {
            vx = v1 + (t_Wavelength - w1) * (v2 - v1) / (w2 - w1);
        }
        else
        {
            vx = v1;   // extrapolating same value for all values out of range
        }

        return vx;
    }

    CSeries CSeries::interpolate(const std::vector<double> & t_Wavelengths) const
    {
        CSeries newProperties;

        if(size() != 0)
        {
            ISeriesPoint * lower = nullptr;
            ISeriesPoint * upper = nullptr;

            for(double wavelength : t_Wavelengths)
            {
                lower = findLower(wavelength);
                upper = findUpper(wavelength);

                if(lower == nullptr)
                {
                    lower = upper;
                }

                if(upper == nullptr)
                {
                    upper = lower;
                }

                newProperties.addProperty(wavelength, interpolate(lower, upper, wavelength));
            }
        }

        return newProperties;
    }

    CSeries CSeries::operator*(const CSeries & other)
    {
        CSeries newProperty;

        const double WAVELENGTHTOLERANCE = 1e-10;

        size_t minSize = std::min(m_Series.size(), other.m_Series.size());

        for(size_t i = 0; i < minSize; ++i)
        {
            double value = m_Series[i]->value() * other.m_Series[i]->value();
            double wv = m_Series[i]->x();
            double testWv = other.m_Series[i]->x();

            if(std::abs(wv - testWv) > WAVELENGTHTOLERANCE)
            {
                throw std::runtime_error(
                  "Wavelengths of two vectors are not the same. Cannot preform multiplication.");
            }
            newProperty.addProperty(wv, value);
        }

        return newProperty;
    }

    CSeries CSeries::operator-(const CSeries & t_Series) const
    {
        const double WAVELENGTHTOLERANCE = 1e-10;

        CSeries newProperties;
        size_t minSize = std::min(m_Series.size(), t_Series.m_Series.size());

        for(size_t i = 0; i < minSize; ++i)
        {
            double value = m_Series[i]->value() - t_Series.m_Series[i]->value();
            double wv = m_Series[i]->x();
            double testWv = t_Series.m_Series[i]->x();

            if(std::abs(wv - testWv) > WAVELENGTHTOLERANCE)
            {
                throw std::runtime_error(
                  "Wavelengths of two vectors are not the same. Cannot preform subtraction.");
            }

            newProperties.addProperty(wv, value);
        }

        return newProperties;
    }

    CSeries operator-(const double val, const CSeries & other)
    {
        CSeries newProperties;

        for(const auto & ot : other)
        {
            double value = val - ot->value();
            double wv = ot->x();

            newProperties.addProperty(wv, value);
        }

        return newProperties;
    }

    CSeries CSeries::operator+(const CSeries & other) const
    {
        const double WAVELENGTHTOLERANCE = 1e-10;

        CSeries newProperties;
        size_t minSize = std::min(m_Series.size(), other.m_Series.size());

        for(size_t i = 0; i < minSize; ++i)
        {
            double value = m_Series[i]->value() + other.m_Series[i]->value();
            double wv = m_Series[i]->x();
            double testWv = other.m_Series[i]->x();

            if(std::abs(wv - testWv) > WAVELENGTHTOLERANCE)
            {
                throw std::runtime_error(
                  "Wavelengths of two vectors are not the same. Cannot preform addition.");
            }

            newProperties.addProperty(wv, value);
        }

        return newProperties;
    }

    std::vector<double> CSeries::getXArray() const
    {
        std::vector<double> aArray;
        for(auto & spectralProperty : m_Series)
        {
            aArray.push_back(spectralProperty->x());
        }

        return aArray;
    }

    double CSeries::sum(double const minLambda, double const maxLambda) const
    {
        double const TOLERANCE = 1e-6;   // introduced because of rounding error
        double total = 0;
        for(auto & aPoint : m_Series)
        {
            double wavelength = aPoint->x();
            // Last point must be excluded because of ranges. Each wavelength represent range from
            // wavelength one to wavelength two. Summing value of the last wavelength in array would
            // be wrong because it would include one additional range after the end of spectrum. For
            // example, summing all the data from 0.38 to 0.78 would include visible range. However,
            // including 0.78 in sum would add extra value from 0.78 to 0.79.
            if(((wavelength >= (minLambda - TOLERANCE) && wavelength < (maxLambda - TOLERANCE))
                || (minLambda == 0 && maxLambda == 0)))
            {
                total += aPoint->value();
            }
        }
        return total;
    }

    void CSeries::sort()
    {
        std::sort(m_Series.begin(),
                  m_Series.end(),
                  [](std::unique_ptr<ISeriesPoint> const & l,
                     std::unique_ptr<ISeriesPoint> const & r) -> bool { return l->x() < r->x(); });
    }

    std::vector<std::unique_ptr<ISeriesPoint>>::const_iterator CSeries::begin() const
    {
        return m_Series.cbegin();
    }

    std::vector<std::unique_ptr<ISeriesPoint>>::const_iterator CSeries::end() const
    {
        return m_Series.cend();
    }

    size_t CSeries::size() const
    {
        return m_Series.size();
    }

    CSeries & CSeries::operator=(CSeries const & t_Series)
    {
        m_Series.clear();
        for(std::unique_ptr<ISeriesPoint> const & val : t_Series.m_Series)
        {
            m_Series.push_back(val->clone());
        }
        return *this;
    }

    ISeriesPoint & CSeries::operator[](size_t Index) const
    {
        if(Index >= m_Series.size())
        {
            throw std::out_of_range("Index out of range.");
        }
        return *m_Series[Index];
    }

    void CSeries::clear()
    {
        m_Series.clear();
    }

    void CSeries::cutExtraData(double minWavelength, double maxWavelength)
    {
        std::vector<std::unique_ptr<ISeriesPoint>> result;
        const auto eps = 1e-8;
        for(const auto & val : m_Series)
        {
            if(val->x() > (minWavelength - eps) && val->x() < (maxWavelength + eps))
            {
                result.push_back(val->clone());
            }
        }

        m_Series.clear();

        for(const auto & val : result)
        {
            m_Series.push_back(val->clone());
        }
    }

}   // namespace FenestrationCommon
