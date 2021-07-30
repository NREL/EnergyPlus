#ifndef SERIES_H
#define SERIES_H

#include <vector>
#include <memory>

namespace FenestrationCommon
{
    // Interface definition for any spectral property. This can be any property that is depending on
    // wavelength (solar intensity, reflectance, transmittance, etc)
    class ISeriesPoint
    {
    public:
        virtual ~ISeriesPoint() = default;

        virtual double x() const = 0;
        virtual double value() const = 0;
        virtual void value(double t_Value) = 0;
        virtual std::unique_ptr<ISeriesPoint> clone() const = 0;
    };

    // Implementation of spectral property interface
    class CSeriesPoint : public ISeriesPoint
    {
    public:
        CSeriesPoint();
        CSeriesPoint(CSeriesPoint const & t_SeriesPoint);
        CSeriesPoint(double t_Wavelength, double t_Value);
        std::unique_ptr<ISeriesPoint> clone() const override;
        double x() const override;
        double value() const override;
        void value(double const t_Value) override;
        CSeriesPoint & operator=(const CSeriesPoint & t_Point);
        bool operator<(const CSeriesPoint & t_Point) const;

    private:
        double m_x;
        double m_Value;
    };

    enum class IntegrationType;

    // Spectral properties for certain range of data. It holds common behavior like integration and
    // interpolation over certain range of data. class CSeries : public
    // std::enable_shared_from_this< CSeries > {
    class CSeries
    {
    public:
        CSeries() = default;

        explicit CSeries(const std::vector<std::pair<double, double>> & t_values);
        explicit CSeries(const std::initializer_list<std::pair<double, double>> & t_values);

        CSeries(CSeries const & t_Series);
        void addProperty(double t_x, double t_Value);
        void insertToBeginning(double t_x, double t_Value);

        // Create wavelength array with identical values over entire wavelength spectrum
        void setConstantValues(const std::vector<double> & t_x, double const t_Value);

        std::unique_ptr<CSeries> integrate(IntegrationType t_IntegrationType,
                                           double normalizationCoefficient = 1) const;
        CSeries interpolate(const std::vector<double> & t_Wavelengths) const;

        //! \brief Multiplication of values in spectral properties that have same wavelength.
        //!
        //! Function will work only if two spectral properties have identical wavelengths. Otherwise
        //! runtime error will be thrown. If two spectral properites do not have same wavelength
        //! range, then interpolation function should be called.
        CSeries operator*(const CSeries & other);

        //! \brief Subtraction of values in spectral properties that have same wavelength.
        //!
        //! Function will work only if two spectral properties have identical wavelengths. Otherwise
        //! runtime error will be thrown. If two spectral properites do not have same wavelength
        //! range, then interpolation function should be called.
        CSeries operator-(const CSeries & other) const;

        //! \brief Addition of values in spectral properties that have same wavelength.
        //!
        //! Function will work only if two spectral properties have identical wavelengths. Otherwise
        //! runtime error will be thrown. If two spectral properties do not have same wavelength
        //! range, then interpolation function should be called.
        CSeries operator+(const CSeries & other) const;

        // Return wavelength values for spectral properties.
        std::vector<double> getXArray() const;

        // Sum of all properties between two x values. Default arguments mean all items are sum
        double sum(double minX = 0, double maxX = 0) const;

        // Sort series by x values in ascending order
        void sort();

        std::vector<std::unique_ptr<ISeriesPoint>>::const_iterator begin() const;
        std::vector<std::unique_ptr<ISeriesPoint>>::const_iterator end() const;
        size_t size() const;

        CSeries & operator=(CSeries const & t_Series);
        ISeriesPoint & operator[](size_t Index) const;

        void clear();

        void cutExtraData(double minWavelength, double maxWavelength);

    private:
        ISeriesPoint * findLower(double t_x) const;
        ISeriesPoint * findUpper(double t_x) const;
        static double interpolate(ISeriesPoint * t_Lower, ISeriesPoint * t_Upper, double t_x);

        std::vector<std::unique_ptr<ISeriesPoint>> m_Series;
    };

    CSeries operator-(const double val, const CSeries & other);

}   // namespace FenestrationCommon

#endif
