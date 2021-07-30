#include <stdexcept>
#include <cassert>

#include "SpectralSample.hpp"
#include "MeasuredSampleData.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;

namespace SpectralAveraging
{
    //////////////////////////////////////////////////////////////////////////////////////
    ////  CSample
    //////////////////////////////////////////////////////////////////////////////////////

    CSample::CSample(const CSeries & t_SourceData,
                     IntegrationType integrationType,
                     double t_NormalizationCoefficient) :
        m_SourceData(t_SourceData),
        m_WavelengthSet(WavelengthSet::Data),
        m_IntegrationType(integrationType),
        m_NormalizationCoefficient(t_NormalizationCoefficient),
        m_StateCalculated(false)
    {
        CSample::reset();
    }

    CSample::CSample() :
        m_WavelengthSet(WavelengthSet::Data),
        m_IntegrationType(IntegrationType::Trapezoidal),
        m_NormalizationCoefficient(1),
        m_StateCalculated(false)
    {
        CSample::reset();
    }

    CSample & CSample::operator=(const CSample & t_Sample)
    {
        m_StateCalculated = t_Sample.m_StateCalculated;
        m_IntegrationType = t_Sample.m_IntegrationType;
        m_NormalizationCoefficient = t_Sample.m_NormalizationCoefficient;
        m_WavelengthSet = t_Sample.m_WavelengthSet;
        m_IncomingSource = t_Sample.m_IncomingSource;
        for(const auto & prop : EnumProperty())
        {
            for(const auto & side : EnumSide())
            {
                m_EnergySource[std::make_pair(prop, side)] =
                  t_Sample.m_EnergySource.at(std::make_pair(prop, side));
            }
        }

        return *this;
    }

    CSample::CSample(const CSample & t_Sample)
    {
        operator=(t_Sample);
    }

    CSeries & CSample::getSourceData()
    {
        calculateState();   // must interpolate data to same wavelengths
        return m_SourceData;
    }

    void CSample::setSourceData(CSeries & t_SourceData)
    {
        m_SourceData = t_SourceData;
        reset();
    }

    void CSample::setDetectorData(const CSeries & t_DetectorData)
    {
        m_DetectorData = t_DetectorData;
        reset();
    }

    FenestrationCommon::IntegrationType CSample::getIntegrator() const
    {
        return m_IntegrationType;
    }

    double CSample::getNormalizationCoeff() const
    {
        return m_NormalizationCoefficient;
    }

    void CSample::assignDetectorAndWavelengths(std::shared_ptr<CSample> const & t_Sample)
    {
        m_DetectorData = t_Sample->m_DetectorData;
        m_Wavelengths = t_Sample->m_Wavelengths;
        m_WavelengthSet = t_Sample->m_WavelengthSet;
    }

    void CSample::setWavelengths(WavelengthSet const t_WavelengthSet,
                                 const std::vector<double> & t_Wavelenghts)
    {
        m_WavelengthSet = t_WavelengthSet;
        switch(t_WavelengthSet)
        {
            case WavelengthSet::Custom:
                m_Wavelengths = t_Wavelenghts;
                break;
            case WavelengthSet::Source:
                if(m_SourceData.size() == 0)
                {
                    throw std::runtime_error(
                      "Cannot extract wavelenghts from source. Source is empty.");
                }
                m_Wavelengths = m_SourceData.getXArray();
                break;
            case WavelengthSet::Data:
                m_Wavelengths = getWavelengthsFromSample();
                break;
            default:
                throw std::runtime_error("Incorrect definition of wavelength set source.");
                break;
        }
        reset();
    }

    double CSample::getEnergy(double const minLambda,
                              double const maxLambda,
                              Property const t_Property,
                              Side const t_Side)
    {
        calculateState();
        return m_EnergySource.at(std::make_pair(t_Property, t_Side)).sum(minLambda, maxLambda);
    }

    std::vector<double> CSample::getWavelengths() const
    {
        return m_Wavelengths;
    }

    double CSample::getProperty(double const minLambda,
                                double const maxLambda,
                                Property const t_Property,
                                Side const t_Side)
    {
        calculateState();
        auto Prop = 0.0;
        // Incoming energy can be calculated only if user has defined incoming source.
        // Otherwise just assume zero property.
        if(m_IncomingSource.size() > 0)
        {
            auto incomingEnergy = m_IncomingSource.sum(minLambda, maxLambda);
            double propertyEnergy =
              m_EnergySource.at(std::make_pair(t_Property, t_Side)).sum(minLambda, maxLambda);
            Prop = propertyEnergy / incomingEnergy;
        }
        return Prop;
    }

    CSeries & CSample::getEnergyProperties(const Property t_Property, const Side t_Side)
    {
        calculateState();
        return m_EnergySource.at(std::make_pair(t_Property, t_Side));
    }

    size_t CSample::getBandSize() const
    {
        return m_Wavelengths.size();
    }

    void CSample::reset()
    {
        m_StateCalculated = false;
        m_IncomingSource = CSeries();
        for(const auto & prop : EnumProperty())
        {
            for(const auto & side : EnumSide())
            {
                m_EnergySource[std::make_pair(prop, side)] = CSeries();
            }
        }
    }

    void CSample::calculateState()
    {
        if(!m_StateCalculated)
        {
            if(m_WavelengthSet != WavelengthSet::Custom)
            {
                setWavelengths(m_WavelengthSet);
            }

            // In case source data are set then apply solar radiation to the calculations.
            // Otherwise, just use measured data.
            if(m_SourceData.size() > 0)
            {
                m_IncomingSource = m_SourceData.interpolate(m_Wavelengths);


                if(m_DetectorData.size() > 0)
                {
                    const auto interpolatedDetector = m_DetectorData.interpolate(m_Wavelengths);
                    m_IncomingSource = m_IncomingSource * interpolatedDetector;
                }

                calculateProperties();

                m_IncomingSource =
                  *m_IncomingSource.integrate(m_IntegrationType, m_NormalizationCoefficient);
                for(const auto & prop : EnumProperty())
                {
                    for(const auto & side : EnumSide())
                    {
                        m_EnergySource[std::make_pair(prop, side)] =
                          *m_EnergySource.at(std::make_pair(prop, side))
                             .integrate(m_IntegrationType, m_NormalizationCoefficient);
                    }
                }

                m_StateCalculated = true;
            }
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////
    ////  CSpectralSample
    //////////////////////////////////////////////////////////////////////////////////////

    CSpectralSample::CSpectralSample(std::shared_ptr<CSpectralSampleData> const & t_SampleData,
                                     const CSeries & t_SourceData,
                                     FenestrationCommon::IntegrationType integrationType,
                                     double NormalizationCoefficient) :
        CSample(t_SourceData, integrationType, NormalizationCoefficient),
        m_SampleData(t_SampleData)
    {
        if(t_SampleData == nullptr)
        {
            throw std::runtime_error("Sample must have measured data.");
        }

        for(const auto & prop : EnumProperty())
        {
            for(const auto & side : EnumSide())
            {
                m_Property[std::make_pair(prop, side)] = CSeries();
            }
        }
    }

    CSpectralSample::CSpectralSample(std::shared_ptr<CSpectralSampleData> const & t_SampleData) :
        CSample(),
        m_SampleData(t_SampleData)
    {
        if(t_SampleData == nullptr)
        {
            throw std::runtime_error("Sample must have measured data.");
        }

        for(const auto & prop : EnumProperty())
        {
            for(const auto & side : EnumSide())
            {
                m_Property[std::make_pair(prop, side)] = CSeries();
            }
        }
    }

    std::shared_ptr<CSpectralSampleData> CSpectralSample::getMeasuredData()
    {
        calculateState();   // Interpolation is needed before returning the data
        return m_SampleData;
    }

    std::vector<double> CSpectralSample::getWavelengthsFromSample() const
    {
        return m_SampleData->getWavelengths();
    }

    CSeries CSpectralSample::getWavelengthsProperty(const Property t_Property, const Side t_Side)
    {
        calculateState();

        return m_Property.at(std::make_pair(t_Property, t_Side));
    }

    void CSpectralSample::calculateProperties()
    {
        for(const auto & prop : EnumProperty())
        {
            for(const auto & side : EnumSide())
            {
                m_Property[std::make_pair(prop, side)] = m_SampleData->properties(prop, side);
                // No need to do interpolation if wavelength set is already from the data.
                if(m_WavelengthSet != WavelengthSet::Data)
                {
                    m_Property[std::make_pair(prop, side)] =
                      m_Property[std::make_pair(prop, side)].interpolate(m_Wavelengths);
                }
            }
        }

        // Calculation of energy balances
        for(const auto & prop : EnumProperty())
        {
            for(const auto & side : EnumSide())
            {
                m_EnergySource[std::make_pair(prop, side)] =
                  m_Property.at(std::make_pair(prop, side)) * m_IncomingSource;
            }
        }
    }

    void CSpectralSample::calculateState()
    {
        CSample::calculateState();
        if(m_SourceData.size() == 0)
        {
            for(const auto & prop : EnumProperty())
            {
                for(const auto & side : EnumSide())
                {
                    m_Property[std::make_pair(prop, side)] = m_SampleData->properties(prop, side);
                }
            }

            m_StateCalculated = true;
        }
    }

    void CSpectralSample::cutExtraData(const double minLambda, const double maxLambda)
    {
        m_SampleData->cutExtraData(minLambda, maxLambda);
    }

    void CSpectralSample::Flipped(bool flipped)
    {
        m_SampleData->Filpped(flipped);
    }

    /////////////////////////////////////////////////////////////////////////////////////
    /// CPhotovoltaicSample
    /////////////////////////////////////////////////////////////////////////////////////

    CPhotovoltaicSample::CPhotovoltaicSample(
      const std::shared_ptr<PhotovoltaicSampleData> & t_PhotovoltaicData,
      const FenestrationCommon::CSeries & t_SourceData,
      FenestrationCommon::IntegrationType integrationType,
      double NormalizationCoefficient) :
        CSpectralSample(
          t_PhotovoltaicData, t_SourceData, integrationType, NormalizationCoefficient),
        m_PCE{{Side::Front, CSeries()}, {Side::Back, CSeries()}},
        m_W{{Side::Front, CSeries()}, {Side::Back, CSeries()}}
    {}

    void CPhotovoltaicSample::calculateState()
    {
        CSpectralSample::calculateProperties();
        for(const auto & side : EnumSide())
        {
            const CSeries eqe{getSample()->pvProperty(side, PVM::EQE)};
            const CSeries voc{getSample()->pvProperty(side, PVM::VOC)};
            const CSeries ff{getSample()->pvProperty(side, PVM::FF)};
            const auto transmittance = m_SampleData->properties(Property::T, side);
            const auto reflectance = m_SampleData->properties(Property::R, side);
            const auto wl = getWavelengthsFromSample();
            CSeries pce;
            CSeries w;
            for(auto i = 0u; i < wl.size(); ++i)
            {
                const double pceVal = pceCalc(wl[i], eqe[i].value(), voc[i].value(), ff[i].value());
                pce.addProperty(wl[i], pceVal);
                w.addProperty(wl[i],
                              1 - pceVal / (1 - transmittance[i].value() - reflectance[i].value()));
            }
            m_PCE[side] = pce;
            m_W[side] = w;
        }
    }

    PhotovoltaicSampleData * CPhotovoltaicSample::getSample() const
    {
        return dynamic_cast<PhotovoltaicSampleData *>(m_SampleData.get());
    }

    double CPhotovoltaicSample::pceCalc(double wavelength, double eqe, double voc, double ff)
    {
        double const microMeterToMeter{1e-6};
        return eqe * voc * ff * wavelength * ConstantsData::ELECTRON_CHARGE * microMeterToMeter
               / (ConstantsData::SPEEDOFLIGHT * ConstantsData::PLANKCONSTANT);
    }

    FenestrationCommon::CSeries & CPhotovoltaicSample::pce(const FenestrationCommon::Side side)
    {
        calculateState();
        return m_PCE.at(side);
    }

    FenestrationCommon::CSeries & CPhotovoltaicSample::w(const FenestrationCommon::Side side)
    {
        calculateState();
        return m_W.at(side);
    }
}   // namespace SpectralAveraging
