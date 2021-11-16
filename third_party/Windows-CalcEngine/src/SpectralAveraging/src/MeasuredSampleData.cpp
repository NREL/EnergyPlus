#include <stdexcept>
#include <cassert>

#include "MeasuredSampleData.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;

namespace SpectralAveraging
{
    ////////////////////////////////////////////////////////////////////////////
    ////     MeasuredRow
    ////////////////////////////////////////////////////////////////////////////
    MeasuredRow::MeasuredRow(double wl, double t, double rf, double rb) :
        wavelength(wl),
        T(t),
        Rf(rf),
        Rb(rb)
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
    /// PVMeasurement
    ///////////////////////////////////////////////////////////////////////////
    PVMeasurement::PVMeasurement(double eqe, double voc, double ff) : EQE(eqe), VOC(voc), FF(ff)
    {}

    ///////////////////////////////////////////////////////////////////////////
    /// PVMeasurementRow
    ///////////////////////////////////////////////////////////////////////////
    PVMeasurementRow::PVMeasurementRow(double wavelength,
                                       const PVMeasurement & front,
                                       const PVMeasurement & back) :
        wavelength(wavelength),
        front(front),
        back(back)
    {}

    ///////////////////////////////////////////////////////////////////////////
    /// PhotovoltaicData
    ///////////////////////////////////////////////////////////////////////////
    PhotovoltaicSampleData::PhotovoltaicSampleData(const CSpectralSampleData & spectralSampleData) :
        CSpectralSampleData(spectralSampleData),
        m_PVData{{{Side::Front, PVM::EQE}, CSeries()},
                 {{Side::Front, PVM::VOC}, CSeries()},
                 {{Side::Front, PVM::FF}, CSeries()},
                 {{Side::Back, PVM::EQE}, CSeries()},
                 {{Side::Back, PVM::VOC}, CSeries()},
                 {{Side::Back, PVM::FF}, CSeries()}}
    {}

    void PhotovoltaicSampleData::addRecord(double m_Wavelength,
                                           const PVMeasurement frontSide,
                                           const PVMeasurement backSide)
    {
        m_PVData.at(std::make_pair(Side::Front, PVM::EQE)).addProperty(m_Wavelength, frontSide.EQE);
        m_PVData.at(std::make_pair(Side::Front, PVM::VOC)).addProperty(m_Wavelength, frontSide.VOC);
        m_PVData.at(std::make_pair(Side::Front, PVM::FF)).addProperty(m_Wavelength, frontSide.FF);
        m_PVData.at(std::make_pair(Side::Back, PVM::EQE)).addProperty(m_Wavelength, backSide.EQE);
        m_PVData.at(std::make_pair(Side::Back, PVM::VOC)).addProperty(m_Wavelength, backSide.VOC);
        m_PVData.at(std::make_pair(Side::Back, PVM::FF)).addProperty(m_Wavelength, backSide.FF);
    }

    PhotovoltaicSampleData::PhotovoltaicSampleData(
      const CSpectralSampleData & spectralSampleData,
      const std::vector<PVMeasurementRow> & pvMeasurements) :
        PhotovoltaicSampleData(spectralSampleData)
    {
        for(const auto & measurement : pvMeasurements)
        {
            addRecord(measurement);
        }

        const std::vector<double> spectralWl{getWavelengths()};
        const std::vector<double> pvWl{
          m_PVData.at(std::make_pair(Side::Front, PVM::EQE)).getXArray()};

        if(spectralWl.size() != pvWl.size())
        {
            throw std::runtime_error("Photovoltaic measurements do not have same amount of data as "
                                     "specular measurements.");
        }

        // Need to check if wavelengths are matching too
        bool wavelengthsMatch{true};

        for(auto i = 0u; i < spectralWl.size(); ++i)
        {
            if(spectralWl[i] != pvWl[i])
            {
                wavelengthsMatch = false;
                break;
            }
        }

        if(!wavelengthsMatch)
        {
            throw std::runtime_error(
              "Wavelengths in spectral and photovoltaic measurements do not match.");
        }
    }

    void PhotovoltaicSampleData::addRecord(const PVMeasurementRow & pvRow)
    {
        addRecord(pvRow.wavelength, pvRow.front, pvRow.back);
    }

    void PhotovoltaicSampleData::interpolate(const std::vector<double> & t_Wavelengths)
    {
        CSpectralSampleData::interpolate(t_Wavelengths);
        for(const auto & side : EnumSide())
        {
            for(const auto & pvm : EnumPVM())
            {
                m_PVData[std::make_pair(side, pvm)] =
                  m_PVData.at(std::make_pair(side, pvm)).interpolate(t_Wavelengths);
            }
        }
    }

    void PhotovoltaicSampleData::cutExtraData(double minLambda, double maxLambda)
    {
        CSpectralSampleData::cutExtraData(minLambda, maxLambda);
        for(const auto & side : EnumSide())
        {
            for(const auto & pvm : EnumPVM())
            {
                m_PVData.at(std::make_pair(side, pvm)).cutExtraData(minLambda, maxLambda);
            }
        }
    }

    FenestrationCommon::CSeries
      PhotovoltaicSampleData::pvProperty(const FenestrationCommon::Side side, const PVM prop) const
    {
        return m_PVData.at(std::make_pair(side, prop));
    }

}   // namespace SpectralAveraging
