#ifndef ANGULARSPECTRALSAMPLE_H
#define ANGULARSPECTRALSAMPLE_H

#include <memory>
#include <vector>

namespace FenestrationCommon
{
    enum class MaterialType;
    enum class Side;
    enum class Property;

    class CSeries;

}   // namespace FenestrationCommon

namespace SpectralAveraging
{
    class CSpectralSampleData;

    class CSpectralSample;

    ////////////////////////////////////////////////////////////////////////////////
    //  CAngularSpectralProperties
    ////////////////////////////////////////////////////////////////////////////////

    // Angular properties for each wavelength of measured data
    class CAngularSpectralProperties
    {
    public:
        CAngularSpectralProperties(std::shared_ptr<CSpectralSample> const & t_SpectralSample,
                                   double const t_Angle,
                                   FenestrationCommon::MaterialType const t_Type,
                                   double const t_Thickness);

        double angle() const;
        std::shared_ptr<CSpectralSampleData> properties() const;

    private:
        void calculateAngularProperties(std::shared_ptr<CSpectralSample> const & t_SpectralSample,
                                        FenestrationCommon::MaterialType const t_Type);

        double m_Angle;
        double m_Thickness;
        std::shared_ptr<CSpectralSampleData> m_AngularData;
    };

    ////////////////////////////////////////////////////////////////////////////////
    //  CSpectralSampleAngle
    ////////////////////////////////////////////////////////////////////////////////

    // Data for spectral sample under certain angle
    class CSpectralSampleAngle
    {
    public:
        CSpectralSampleAngle(std::shared_ptr<CSpectralSample> const & t_Sample,
                             double const t_Angle);

        double angle() const;
        std::shared_ptr<CSpectralSample> sample() const;

    protected:
        std::shared_ptr<CSpectralSample> m_Sample;
        double m_Angle;
    };

    ////////////////////////////////////////////////////////////////////////////////
    //  CAngularSpectralSample
    ////////////////////////////////////////////////////////////////////////////////

    class CAngularSpectralSample
    {
    public:
        CAngularSpectralSample(const std::shared_ptr<CSpectralSample> & t_SpectralSample,
                               double t_Thickness,
                               FenestrationCommon::MaterialType t_Type);

        void setSourceData(FenestrationCommon::CSeries & t_SourceData);
        void setDetectorData(FenestrationCommon::CSeries & t_DetectorData);

        // Get averaged property over the wavelength
        double getProperty(double minLambda,
                           double maxLambda,
                           FenestrationCommon::Property t_Property,
                           FenestrationCommon::Side t_Side,
                           double t_Angle);

        // Get property at each wavelength
        std::vector<double> getWavelengthsProperty(double minLambda,
                                                   double maxLambda,
                                                   FenestrationCommon::Property t_Property,
                                                   FenestrationCommon::Side t_Side,
                                                   double t_Angle);

        std::vector<double> getBandWavelengths() const;
        void setBandWavelengths(const std::vector<double> & wavelengths);

        void Flipped(bool flipped);

    protected:
        // Finds spectral sample or creates new one if sample is not already created
        std::shared_ptr<CSpectralSample> findSpectralSample(double t_Angle);

    private:
        std::vector<std::shared_ptr<CSpectralSampleAngle>> m_SpectralProperties;
        std::shared_ptr<CSpectralSample> m_SpectralSampleZero;   // spectral sample as zero degrees
        double m_Thickness;
        FenestrationCommon::MaterialType m_Type;
    };

}   // namespace SpectralAveraging

#endif
