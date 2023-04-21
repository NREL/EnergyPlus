#include <vector>
#include <algorithm>
#include <cassert>
#include <cmath>

#include "AngularSpectralSample.hpp"
#include "MeasuredSampleData.hpp"
#include "SpectralSample.hpp"
#include "AngularProperties.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;

namespace SpectralAveraging
{
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //// CAngularSpectralProperties
    ////////////////////////////////////////////////////////////////////////////////////////////////////

    CAngularSpectralProperties::CAngularSpectralProperties(
      std::shared_ptr<CSpectralSample> const & t_SpectralSample,
      double const t_Angle,
      MaterialType const t_Type,
      double const t_Thickness) :
        m_Angle(t_Angle),
        m_Thickness(t_Thickness)
    {
        m_AngularData = std::make_shared<CSpectralSampleData>();
        calculateAngularProperties(t_SpectralSample, t_Type);
    }

    double CAngularSpectralProperties::angle() const
    {
        return m_Angle;
    }

    std::shared_ptr<CSpectralSampleData> CAngularSpectralProperties::properties() const
    {
        return m_AngularData;
    }

    void CAngularSpectralProperties::calculateAngularProperties(
      std::shared_ptr<CSpectralSample> const & t_SpectralSample, MaterialType const t_Type)
    {
        assert(t_SpectralSample != nullptr);

        auto aMeasuredData = t_SpectralSample->getMeasuredData();
        auto aWavelengths = t_SpectralSample->getWavelengths();

        // Spectral sample is not initialized yet, use wavelengths from measured data instead.
        if(aWavelengths.empty())
        {
            aWavelengths = aMeasuredData->getWavelengths();
        }

        if(m_Angle != 0)
        {
            auto aSourceData = t_SpectralSample->getSourceData();


            const auto aT =
              aMeasuredData->properties(Property ::T, Side::Front).interpolate(aWavelengths);
            assert(aT.size() == aWavelengths.size());

            const auto aRf =
              aMeasuredData->properties(Property::R, Side::Front).interpolate(aWavelengths);
            assert(aRf.size() == aWavelengths.size());

            const auto aRb =
              aMeasuredData->properties(Property::R, Side::Back).interpolate(aWavelengths);
            assert(aRb.size() == aWavelengths.size());

            const auto lowLambda = 0.3;
            const auto highLambda = 2.5;

            // TODO: Only one side is measured and it is considered that front properties are equal
            // to back properties
            const auto aTSolNorm =
              t_SpectralSample->getProperty(lowLambda, highLambda, Property::T, Side::Front);

            for(size_t i = 0; i < aWavelengths.size(); ++i)
            {
                const auto ww = aWavelengths[i] * 1e-6;
                const auto T = aT[i].value();
                const auto Rf = aRf[i].value();
                const auto Rb = aRb[i].value();

                const auto aSurfaceType = coatingType.at(t_Type);

                auto aFrontFactory = CAngularPropertiesFactory(T, Rf, m_Thickness, aTSolNorm);
                auto aBackFactory = CAngularPropertiesFactory(T, Rb, m_Thickness, aTSolNorm);

                auto aFrontProperties = aFrontFactory.getAngularProperties(aSurfaceType);
                auto aBackProperties = aBackFactory.getAngularProperties(aSurfaceType);

                const auto Tangle = aFrontProperties->transmittance(m_Angle, ww);
                const auto Rfangle = aFrontProperties->reflectance(m_Angle, ww);
                const auto Rbangle = aBackProperties->reflectance(m_Angle, ww);

                m_AngularData->addRecord(ww * 1e6, Tangle, Rfangle, Rbangle);
            }
        }
        else
        {
            m_AngularData = aMeasuredData;
            m_AngularData->interpolate(aWavelengths);
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //// CSpectralSampleAngle
    ////////////////////////////////////////////////////////////////////////////////////////////////////

    CSpectralSampleAngle::CSpectralSampleAngle(std::shared_ptr<CSpectralSample> const & t_Sample,
                                               double const t_Angle) :
        m_Sample(t_Sample),
        m_Angle(t_Angle)
    {}

    double CSpectralSampleAngle::angle() const
    {
        return m_Angle;
    }

    std::shared_ptr<CSpectralSample> CSpectralSampleAngle::sample() const
    {
        return m_Sample;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //// CAngularSpectralSample
    ////////////////////////////////////////////////////////////////////////////////////////////////////

    CAngularSpectralSample::CAngularSpectralSample(
      std::shared_ptr<CSpectralSample> const & t_SpectralSample,
      double const t_Thickness,
      FenestrationCommon::MaterialType const t_Type) :
        m_SpectralSampleZero(t_SpectralSample),
        m_Thickness(t_Thickness),
        m_Type(t_Type)
    {}

    void CAngularSpectralSample::setSourceData(CSeries & t_SourceData)
    {
        m_SpectralSampleZero->setSourceData(t_SourceData);
        m_SpectralProperties.clear();
    }

    void CAngularSpectralSample::setDetectorData(CSeries & t_DetectorData)
    {
        m_SpectralSampleZero->setDetectorData(t_DetectorData);
        m_SpectralProperties.clear();
    }

    double CAngularSpectralSample::getProperty(double const minLambda,
                                               double const maxLambda,
                                               Property const t_Property,
                                               Side const t_Side,
                                               double const t_Angle)
    {
        auto aSample = findSpectralSample(t_Angle);
        return aSample->getProperty(minLambda, maxLambda, t_Property, t_Side);
    }

    std::vector<double> CAngularSpectralSample::getWavelengthsProperty(double const minLambda,
                                                                       double const maxLambda,
                                                                       Property const t_Property,
                                                                       Side const t_Side,
                                                                       double const t_Angle)
    {
        auto aSample = findSpectralSample(t_Angle);

        auto aProperties = aSample->getWavelengthsProperty(t_Property, t_Side);

        std::vector<double> aValues;

        for(auto & aProperty : aProperties)
        {
            if(aProperty->x() >= (minLambda - ConstantsData::floatErrorTolerance)
               && aProperty->x() <= (maxLambda + ConstantsData::floatErrorTolerance))
            {
                aValues.push_back(aProperty->value());
            }
        }

        return aValues;
    }

    std::vector<double> CAngularSpectralSample::getBandWavelengths() const
    {
        return m_SpectralSampleZero->getWavelengthsFromSample();
    }

    void CAngularSpectralSample::setBandWavelengths(const std::vector<double> & wavelegths)
    {
        m_SpectralSampleZero->setWavelengths(WavelengthSet::Custom, wavelegths);
    }

    void CAngularSpectralSample::Flipped(bool flipped)
    {
        m_SpectralSampleZero->Flipped(flipped);
        for(auto & val : m_SpectralProperties)
        {
            val->sample()->Flipped(flipped);
        }
    }

    std::shared_ptr<CSpectralSample>
      CAngularSpectralSample::findSpectralSample(double const t_Angle)
    {
        std::shared_ptr<CSpectralSample> aSample = nullptr;

        const auto it = find_if(m_SpectralProperties.begin(),
                                m_SpectralProperties.end(),
                                [&t_Angle](std::shared_ptr<CSpectralSampleAngle> const & obj) {
                                    return std::abs(obj->angle() - t_Angle) < 1e-6;
                                });

        if(it != m_SpectralProperties.end())
        {
            aSample = (*it)->sample();
        }
        else
        {
            auto aAngularData =
              CAngularSpectralProperties(m_SpectralSampleZero, t_Angle, m_Type, m_Thickness);

            aSample =
              std::make_shared<CSpectralSample>(aAngularData.properties(),
                                                m_SpectralSampleZero->getSourceData(),
                                                m_SpectralSampleZero->getIntegrator(),
                                                m_SpectralSampleZero->getNormalizationCoeff());
            aSample->assignDetectorAndWavelengths(m_SpectralSampleZero);
            const auto aSpectralSampleAngle =
              std::make_shared<CSpectralSampleAngle>(aSample, t_Angle);
            m_SpectralProperties.push_back(aSpectralSampleAngle);
        }

        return aSample;
    }

}   // namespace SpectralAveraging
