#include <stdexcept>
#include <cassert>
#include <utility>

#include "MeasuredSampleData.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;

namespace SpectralAveraging
{
    ////////////////////////////////////////////////////////////////////////////
    ////     MeasuredRow
    ////////////////////////////////////////////////////////////////////////////
    MeasuredRow::MeasuredRow(double wl, double t, double rf, double rb) :
        wavelength(wl), T(t), Rf(rf), Rb(rb)
    {}

    ////////////////////////////////////////////////////////////////////////////
    ////     SampleData
    ////////////////////////////////////////////////////////////////////////////

    SampleData::SampleData() : m_Flipped(false)
    {}

    bool SampleData::Flipped() const
    {
        return m_Flipped;
    }

    void SampleData::Filpped(bool t_Flipped)
    {
        m_Flipped = t_Flipped;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////     CSpectralSampleData
    ////////////////////////////////////////////////////////////////////////////

    CSpectralSampleData::CSpectralSampleData() : SampleData(), m_absCalculated(false)
    {
        for(const auto & prop : EnumProperty())
        {
            for(const auto & side : EnumSide())
            {
                m_Property[std::make_pair(prop, side)] = CSeries();
            }
        }
    }

    void CSpectralSampleData::addRecord(double const t_Wavelength,
                                        double const t_Transmittance,
                                        double const t_ReflectanceFront,
                                        double const t_ReflectanceBack)
    {
        m_Property.at(std::make_pair(Property::T, Side::Front))
          .addProperty(t_Wavelength, t_Transmittance);
        m_Property.at(std::make_pair(Property::T, Side::Back))
          .addProperty(t_Wavelength, t_Transmittance);
        m_Property.at(std::make_pair(Property::R, Side::Front))
          .addProperty(t_Wavelength, t_ReflectanceFront);
        m_Property.at(std::make_pair(Property::R, Side::Back))
          .addProperty(t_Wavelength, t_ReflectanceBack);
        reset();
    }


    CSpectralSampleData::CSpectralSampleData(const std::vector<MeasuredRow> & tValues) :
        CSpectralSampleData()
    {
        m_Property.at(std::make_pair(Property::T, Side::Front)).clear();
        m_Property.at(std::make_pair(Property::T, Side::Back)).clear();
        m_Property.at(std::make_pair(Property::R, Side::Front)).clear();
        m_Property.at(std::make_pair(Property::R, Side::Back)).clear();
        for(const auto & val : tValues)
        {
            m_Property.at(std::make_pair(Property::T, Side::Front))
              .addProperty(val.wavelength, val.T);
            m_Property.at(std::make_pair(Property::T, Side::Back))
              .addProperty(val.wavelength, val.T);
            m_Property.at(std::make_pair(Property::R, Side::Front))
              .addProperty(val.wavelength, val.Rf);
            m_Property.at(std::make_pair(Property::R, Side::Back))
              .addProperty(val.wavelength, val.Rb);
        }
    }

    CSeries & CSpectralSampleData::properties(FenestrationCommon::Property prop,
                                              FenestrationCommon::Side side)
    {
        calculateProperties();
        auto aSide = FenestrationCommon::getSide(side, m_Flipped);
        return m_Property.at(std::make_pair(prop, aSide));
    }

    std::vector<double> CSpectralSampleData::getWavelengths() const
    {
        return m_Property.at(std::make_pair(Property::T, Side::Front)).getXArray();
    }

    // Interpolate current sample data to new wavelengths set
    void CSpectralSampleData::interpolate(std::vector<double> const & t_Wavelengths)
    {
        for(const auto & prop : EnumProperty())
        {
            for(const auto & side : EnumSide())
            {
                m_Property[std::make_pair(prop, side)] =
                  m_Property.at(std::make_pair(prop, side)).interpolate(t_Wavelengths);
            }
        }
    }

    void CSpectralSampleData::reset()
    {
        m_absCalculated = false;
    }

    void CSpectralSampleData::calculateProperties()
    {
        if(!m_absCalculated)
        {
            m_Property.at(std::make_pair(Property::Abs, Side::Front)).clear();
            m_Property.at(std::make_pair(Property::Abs, Side::Back)).clear();

            const auto wv = m_Property.at(std::make_pair(Property::T, Side::Front)).getXArray();

            for(size_t i = 0; i < wv.size(); ++i)
            {
                auto RFrontSide = m_Flipped ? Side::Back : Side::Front;
                auto RBackSide = m_Flipped ? Side::Front : Side::Back;
                auto value = 1 - m_Property.at(std::make_pair(Property::T, Side::Front))[i].value()
                             - m_Property.at(std::make_pair(Property::R, RFrontSide))[i].value();
                m_Property.at(std::make_pair(Property::Abs, Side::Front)).addProperty(wv[i], value);

                value = 1 - m_Property.at(std::make_pair(Property::T, Side::Back))[i].value()
                        - m_Property.at(std::make_pair(Property::R, RBackSide))[i].value();
                m_Property.at(std::make_pair(Property::Abs, Side::Back)).addProperty(wv[i], value);
            }
            m_absCalculated = true;
        }
    }


    std::shared_ptr<CSpectralSampleData>
      CSpectralSampleData::create(const std::vector<MeasuredRow> & tValues)
    {
        return std::shared_ptr<CSpectralSampleData>(new CSpectralSampleData(tValues));
    }

    std::shared_ptr<CSpectralSampleData> CSpectralSampleData::create()
    {
        return CSpectralSampleData::create({});
    }

    void CSpectralSampleData::cutExtraData(const double minLambda, const double maxLambda)
    {
        for(const auto & side : EnumSide())
        {
            m_Property.at(std::make_pair(Property::T, side)).cutExtraData(minLambda, maxLambda);
            m_Property.at(std::make_pair(Property::R, side)).cutExtraData(minLambda, maxLambda);
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// PhotovoltaicData
    ///////////////////////////////////////////////////////////////////////////
    PhotovoltaicSampleData::PhotovoltaicSampleData(const CSpectralSampleData & spectralSampleData) :
        CSpectralSampleData(spectralSampleData),
        m_EQE{{Side::Front, CSeries()}, {Side::Back, CSeries()}}
    {}

    PhotovoltaicSampleData::PhotovoltaicSampleData(const CSpectralSampleData & spectralSampleData,
                                                   const CSeries & eqeValuesFront,
                                                   const CSeries & eqeValuesBack) :
        CSpectralSampleData(spectralSampleData),
        m_EQE{{Side::Front, eqeValuesFront}, {Side::Back, eqeValuesBack}}
    {
        const auto spectralWl{getWavelengths()};
        for(const auto side : FenestrationCommon::EnumSide())
        {
            const auto eqeWavelengths{m_EQE.at(side).getXArray()};
            if(spectralWl.size() != eqeWavelengths.size())
            {
                throw std::runtime_error("Measured spectral data do not have same amount of data "
                                         "as provided eqe values for the photovoltaic.");
            }
            for(size_t i = 0u; i < spectralWl.size(); ++i)
            {
                if(spectralWl[i] != eqeWavelengths[i])
                {
                    throw std::runtime_error("Measured spectral wavelengths are not matching to "
                                             "provided eqe photovoltaic wavelengths.");
                }
            }
        }
    }

    void PhotovoltaicSampleData::cutExtraData(double minLambda, double maxLambda)
    {
        CSpectralSampleData::cutExtraData(minLambda, maxLambda);
        for(const auto & side : EnumSide())
        {
            m_EQE.at(side).cutExtraData(minLambda, maxLambda);
        }
    }

    CSeries PhotovoltaicSampleData::eqe(const Side side) const
    {
        return m_EQE.at(side);
    }

}   // namespace SpectralAveraging
