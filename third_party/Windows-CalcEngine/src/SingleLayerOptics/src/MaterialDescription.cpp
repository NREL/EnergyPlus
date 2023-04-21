#include <cassert>
#include <stdexcept>
#include <sstream>

#include "MaterialDescription.hpp"
#include "WCECommon.hpp"
#include "WCESpectralAveraging.hpp"
#include "OpticalSurface.hpp"

using namespace FenestrationCommon;
using namespace SpectralAveraging;

namespace SingleLayerOptics
{
    double modifyProperty(const double t_Range, const double t_Solar, const double t_Fraction)
    {
        // If t_fraction == 1 that means a dual-band material is evaluated only for partial range
        if(t_Fraction == 1)
        {
            return t_Range;
        }
        else
        {
            auto ratio{(t_Solar - t_Fraction * t_Range) / (1 - t_Fraction)};
            if(ratio > 1)
                ratio = 1;
            if(ratio < 0)
                ratio = 0;
            return ratio;
        }
    }

    std::vector<std::vector<double>>
      modifyProperties(std::vector<std::vector<double>> const & t_PartialRange,
                       std::vector<std::vector<double>> const & t_FullRange,
                       const double t_Fraction)
    {
        // Creating a vector with the correct size already reserved
        std::vector<double> outgoing;
        size_t outgoingSize = t_PartialRange.begin()->size();
        outgoing.resize(outgoingSize);
        size_t incomingSize = t_PartialRange.size();
        std::vector<std::vector<double>> modifiedValues(incomingSize, outgoing);
        for(size_t i = 0; i < incomingSize; ++i)
        {
            std::vector<double> const & partialOutgoing = t_PartialRange.at(i);
            std::vector<double> const & fullOutgoing = t_FullRange.at(i);
            std::vector<double> & modifiedOutgoing = modifiedValues.at(i);

            for(size_t j = 0; j < outgoingSize; ++j)
            {
                modifiedOutgoing[j] =
                  modifyProperty(partialOutgoing[j], fullOutgoing[j], t_Fraction);
            }
        }
        return modifiedValues;
    }

    std::vector<std::shared_ptr<CMaterial>>
      createNIRRange(const std::shared_ptr<CMaterial> & t_PartialRange,
                     const std::shared_ptr<CMaterial> & t_FullRange,
                     const double t_Fraction)
    {
        std::vector<std::shared_ptr<CMaterial>> materials;

        double Tf_nir = modifyProperty(t_PartialRange->getProperty(Property::T, Side::Front),
                                       t_FullRange->getProperty(Property::T, Side::Front),
                                       t_Fraction);
        double Tb_nir = modifyProperty(t_PartialRange->getProperty(Property::T, Side::Back),
                                       t_FullRange->getProperty(Property::T, Side::Back),
                                       t_Fraction);

        double Rf_nir = modifyProperty(t_PartialRange->getProperty(Property::R, Side::Front),
                                       t_FullRange->getProperty(Property::R, Side::Front),
                                       t_Fraction);
        double Rb_nir = modifyProperty(t_PartialRange->getProperty(Property::R, Side::Back),
                                       t_FullRange->getProperty(Property::R, Side::Back),
                                       t_Fraction);

        double minRangeLambda = t_PartialRange->getMinLambda();

        if(minRangeLambda > 0.32)
        {
            std::shared_ptr<CMaterialSingleBand> aMaterial = std::make_shared<CMaterialSingleBand>(
              Tf_nir, Tb_nir, Rf_nir, Rb_nir, 0.32, minRangeLambda);
            materials.push_back(aMaterial);
        }

        materials.push_back(t_PartialRange);

        double maxRangeLambda = t_PartialRange->getMaxLambda();
        std::shared_ptr<CMaterialSingleBand> aMaterial = std::make_shared<CMaterialSingleBand>(
          Tf_nir, Tb_nir, Rf_nir, Rb_nir, maxRangeLambda, 2.5);
        materials.push_back(aMaterial);
        return materials;
    }

    std::vector<std::shared_ptr<CMaterial>>
      createNIRRange(const std::shared_ptr<CMaterialSingleBandBSDF> & t_PartialRange,
                     const std::shared_ptr<CMaterialSingleBandBSDF> & t_FullRange,
                     const double t_Fraction)
    {
        std::vector<std::shared_ptr<CMaterial>> materials;

        auto Tf_nir = modifyProperties(t_PartialRange->getBSDFMatrix(Property::T, Side::Front),
                                       t_FullRange->getBSDFMatrix(Property::T, Side::Front),
                                       t_Fraction);
        auto Tb_nir = modifyProperties(t_PartialRange->getBSDFMatrix(Property::T, Side::Back),
                                       t_FullRange->getBSDFMatrix(Property::T, Side::Back),
                                       t_Fraction);

        auto Rf_nir = modifyProperties(t_PartialRange->getBSDFMatrix(Property::R, Side::Front),
                                       t_FullRange->getBSDFMatrix(Property::R, Side::Front),
                                       t_Fraction);
        auto Rb_nir = modifyProperties(t_PartialRange->getBSDFMatrix(Property::R, Side::Back),
                                       t_FullRange->getBSDFMatrix(Property::R, Side::Back),
                                       t_Fraction);

        double minRangeLambda = t_PartialRange->getMinLambda();

        if(minRangeLambda > 0.32)
        {
            std::shared_ptr<CMaterialSingleBandBSDF> aMaterial =
              std::make_shared<CMaterialSingleBandBSDF>(Tf_nir,
                                                        Tb_nir,
                                                        Rf_nir,
                                                        Rb_nir,
                                                        t_PartialRange->getHemisphere(),
                                                        0.32,
                                                        minRangeLambda);
            materials.push_back(aMaterial);
        }

        materials.push_back(t_PartialRange);

        double maxRangeLambda = t_PartialRange->getMaxLambda();
        std::shared_ptr<CMaterialSingleBandBSDF> aMaterial =
          std::make_shared<CMaterialSingleBandBSDF>(
            Tf_nir, Tb_nir, Rf_nir, Rb_nir, t_PartialRange->getHemisphere(), maxRangeLambda, 2.5);
        materials.push_back(aMaterial);
        return materials;
    }

    ////////////////////////////////////////////////////////////////////////////////////
    ////   RMaterialProperties
    ////////////////////////////////////////////////////////////////////////////////////

    RMaterialProperties::RMaterialProperties(const double aTf,
                                             const double aTb,
                                             const double aRf,
                                             const double aRb)
    {
        m_Surface[Side::Front] = std::make_shared<CSurface>(aTf, aRf);
        m_Surface[Side::Back] = std::make_shared<CSurface>(aTb, aRb);
    }

    double RMaterialProperties::getProperty(const Property t_Property, const Side t_Side) const
    {
        return m_Surface.at(t_Side)->getProperty(t_Property);
    }

    ////////////////////////////////////////////////////////////////////////////////////
    ////   CMaterial
    ////////////////////////////////////////////////////////////////////////////////////

    CMaterial::CMaterial(const double minLambda, const double maxLambda) :
        m_MinLambda(minLambda), m_MaxLambda(maxLambda), m_WavelengthsCalculated(false)
    {}

    CMaterial::CMaterial(const WavelengthRange t_Range) : m_WavelengthsCalculated(false)
    {
        CWavelengthRange aRange = CWavelengthRange(t_Range);
        m_MinLambda = aRange.minLambda();
        m_MaxLambda = aRange.maxLambda();
    }

    void CMaterial::setSourceData(CSeries &)
    {
        // Default material will not have source data
    }

    void CMaterial::setDetectorData(FenestrationCommon::CSeries &)
    {
        // Default material will not have detector data
    }

    std::vector<RMaterialProperties> CMaterial::getBandProperties()
    {
        std::vector<RMaterialProperties> aProperties;

        std::vector<double> Tf = getBandProperties(Property::T, Side::Front);
        std::vector<double> Tb = getBandProperties(Property::T, Side::Back);
        std::vector<double> Rf = getBandProperties(Property::R, Side::Front);
        std::vector<double> Rb = getBandProperties(Property::R, Side::Back);

        // It is necessary to skip calculations if solar properties are not assigned yet
        const size_t size = getBandSize();
        for(size_t i = 0; i < size; ++i)
        {
            RMaterialProperties aMaterial = RMaterialProperties(Tf[i], Tb[i], Rf[i], Rb[i]);
            aProperties.push_back(aMaterial);
        }

        return aProperties;
    }

    std::shared_ptr<CSpectralSample> CMaterial::getSpectralSample()
    {
        std::vector<double> Tf = getBandProperties(Property::T, Side::Front);
        std::vector<double> Rf = getBandProperties(Property::R, Side::Front);
        std::vector<double> Rb = getBandProperties(Property::R, Side::Back);

        std::shared_ptr<CSpectralSampleData> aSampleData = std::make_shared<CSpectralSampleData>();

        size_t size = getBandSize();
        for(size_t i = 0; i < size; ++i)
        {
            aSampleData->addRecord(m_Wavelengths[i], Tf[i], Rf[i], Rb[i]);
        }

        return std::make_shared<CSpectralSample>(aSampleData);
    }

    std::vector<double> CMaterial::getBandWavelengths()
    {
        if(!m_WavelengthsCalculated)
        {
            m_Wavelengths = calculateBandWavelengths();
        }
        return m_Wavelengths;
    }

    bool CMaterial::isWavelengthInRange(double wavelength) const
    {
        return ((m_MinLambda - ConstantsData::wavelengthErrorTolerance) <= wavelength)
               && ((m_MaxLambda + ConstantsData::wavelengthErrorTolerance) >= wavelength);
    }

    std::vector<double>
      CMaterial::trimWavelengthToRange(const std::vector<double> & wavelengths) const
    {
        std::vector<double> wl;

        for(const auto & w : wavelengths)
        {
            if(w > (m_MinLambda - ConstantsData::floatErrorTolerance)
               && (w < (m_MaxLambda + ConstantsData::floatErrorTolerance)))
            {
                wl.push_back(w);
            }
        }

        return wl;
    }

    void CMaterial::setBandWavelengths(const std::vector<double> & wavelengths)
    {
        // Trimming is necessary in order to keep data within integration range
        m_Wavelengths = trimWavelengthToRange(wavelengths);
        m_WavelengthsCalculated = true;
    }

    size_t CMaterial::getBandSize()
    {
        return getBandWavelengths().size();
    }

    int CMaterial::getBandIndex(const double t_Wavelength)
    {
        int aIndex = -1;
        size_t size = getBandSize();
        for(size_t i = 0; i < size; ++i)
        {
            if(m_Wavelengths[i] < (t_Wavelength + 1e-6))
            {
                ++aIndex;
            }
        }
        return aIndex;
    }

    double CMaterial::getMinLambda() const
    {
        return m_MinLambda;
    }

    double CMaterial::getMaxLambda() const
    {
        return m_MaxLambda;
    }

    void CMaterial::Flipped(bool)
    {
        // Does nothing so far. Needs to be virtual once shadings are resolved.
    }

    ////////////////////////////////////////////////////////////////////////////////////
    ////   CMaterialSingleBand
    ////////////////////////////////////////////////////////////////////////////////////
    CMaterialSingleBand::CMaterialSingleBand(const double t_Tf,
                                             const double t_Tb,
                                             const double t_Rf,
                                             const double t_Rb,
                                             const double minLambda,
                                             const double maxLambda) :
        CMaterial(minLambda, maxLambda)
    {
        m_Property[Side::Front] = std::make_shared<CSurface>(t_Tf, t_Rf);
        m_Property[Side::Back] = std::make_shared<CSurface>(t_Tb, t_Rb);
    }

    CMaterialSingleBand::CMaterialSingleBand(const double t_Tf,
                                             const double t_Tb,
                                             const double t_Rf,
                                             const double t_Rb,
                                             const WavelengthRange t_Range) :
        CMaterial(t_Range)
    {
        m_Property[Side::Front] = std::make_shared<CSurface>(t_Tf, t_Rf);
        m_Property[Side::Back] = std::make_shared<CSurface>(t_Tb, t_Rb);
    }

    double CMaterialSingleBand::getProperty(Property t_Property,
                                            Side t_Side,
                                            const CBeamDirection &,
                                            const CBeamDirection &) const
    {
        return m_Property.at(t_Side)->getProperty(t_Property);
    }

    std::vector<double> CMaterialSingleBand::getBandProperties(const Property t_Property,
                                                               const Side t_Side,
                                                               const CBeamDirection &,
                                                               const CBeamDirection &) const
    {
        std::vector<double> aResult;
        const auto prop{getProperty(t_Property, t_Side)};
        aResult.push_back(prop);
        aResult.push_back(prop);
        return aResult;
    }

    std::vector<double> CMaterialSingleBand::calculateBandWavelengths()
    {
        std::vector<double> aWavelengths;
        aWavelengths.push_back(m_MinLambda);
        aWavelengths.push_back(m_MaxLambda);
        return aWavelengths;
    }

    ////////////////////////////////////////////////////////////////////////////////////
    ////   CMaterialDualBand
    ////////////////////////////////////////////////////////////////////////////////////

    IMaterialDualBand::IMaterialDualBand(const std::shared_ptr<CMaterial> & t_PartialRange,
                                         const std::shared_ptr<CMaterial> & t_FullRange,
                                         double t_Ratio) :
        CMaterial(t_FullRange->getMinLambda(), t_FullRange->getMaxLambda()),
        m_MaterialFullRange(t_FullRange),
        m_MaterialPartialRange(t_PartialRange),
        m_RangeCreator(std::bind(&IMaterialDualBand::createRangesFromRatio, this, t_Ratio))
    {}

    IMaterialDualBand::IMaterialDualBand(const std::shared_ptr<CMaterial> & t_PartialRange,
                                         const std::shared_ptr<CMaterial> & t_FullRange,
                                         const FenestrationCommon::CSeries & t_SolarRadiation) :
        CMaterial(t_FullRange->getMinLambda(), t_FullRange->getMaxLambda()),
        m_MaterialFullRange(t_FullRange),
        m_MaterialPartialRange(t_PartialRange),
        m_RangeCreator(
          std::bind(&IMaterialDualBand::createRangesFromSolarRadiation, this, t_SolarRadiation))
    {}

    void IMaterialDualBand::setSourceData(CSeries & t_SourceData)
    {
        m_Materials.clear();
        m_MaterialFullRange->setSourceData(t_SourceData);
        m_MaterialPartialRange->setSourceData(t_SourceData);
        checkIfMaterialWithingSolarRange(*m_MaterialPartialRange);
        createUVRange();
        double lowLambda = m_MaterialPartialRange->getMinLambda();
        double highLambda = m_MaterialPartialRange->getMaxLambda();
        CNIRRatio nirRatio = CNIRRatio(t_SourceData, lowLambda, highLambda);
        createNIRRange(m_MaterialPartialRange, m_MaterialFullRange, NIRRatio);
    }

    void IMaterialDualBand::setDetectorData(FenestrationCommon::CSeries & t_DetectorData)
    {
        m_MaterialFullRange->setDetectorData(t_DetectorData);
        m_MaterialPartialRange->setDetectorData(t_DetectorData);
    }

    double IMaterialDualBand::getProperty(Property t_Property,
                                          Side t_Side,
                                          const CBeamDirection & t_Incoming,
                                          const CBeamDirection & t_Outgoing) const
    {
        return m_MaterialFullRange->getProperty(t_Property, t_Side, t_Incoming, t_Outgoing);
    }

    std::vector<double>
      IMaterialDualBand::getBandProperties(const Property t_Property,
                                           const Side t_Side,
                                           const CBeamDirection & t_Incoming,
                                           const CBeamDirection & t_Outgoing) const
    {
        m_RangeCreator();
        std::vector<double> aResults;

        for(const auto wl : m_Wavelengths)
        {
            aResults.emplace_back(getMaterialFromWavelegth(wl)->getProperty(
              t_Property, t_Side, t_Incoming, t_Outgoing));
        }

        return aResults;
    }

    std::vector<double> IMaterialDualBand::calculateBandWavelengths()
    {
        m_RangeCreator();
        std::vector<double> aWavelengths;
        size_t size = m_Materials.size();
        for(size_t i = 0; i < size; ++i)
        {
            aWavelengths.push_back(m_Materials[i]->getMinLambda());
        }

        // aWavelengths.push_back(m_Materials.back()->getMaxLambda());

        return aWavelengths;
    }

    void IMaterialDualBand::checkIfMaterialWithingSolarRange(const CMaterial & t_Material) const
    {
        const double lowLambda = t_Material.getMinLambda();
        const double highLambda = t_Material.getMaxLambda();
        if(lowLambda < 0.32 || highLambda < 0.32 || lowLambda > 2.5 || highLambda > 2.5)
        {
            throw std::runtime_error("Material properties out of range. Wavelength range must be "
                                     "between 0.32 and 2.5 microns.");
        }
    }

    void IMaterialDualBand::createUVRange()
    {
        double T = 0;
        double R = 0;
        double minLambda = 0.3;
        double maxLambda = 0.32;
        std::shared_ptr<CMaterial> aUVMaterial =
          std::make_shared<CMaterialSingleBand>(T, T, R, R, minLambda, maxLambda);
        m_Materials.push_back(aUVMaterial);
    }

    CMaterialDualBand::CMaterialDualBand(const std::shared_ptr<CMaterial> & t_PartialRange,
                                         const std::shared_ptr<CMaterial> & t_FullRange,
                                         double t_Ratio) :
        IMaterialDualBand(t_PartialRange, t_FullRange, t_Ratio)
    {}

    CMaterialDualBand::CMaterialDualBand(const std::shared_ptr<CMaterial> & t_PartialRange,
                                         const std::shared_ptr<CMaterial> & t_FullRange,
                                         const FenestrationCommon::CSeries & t_SolarRadiation) :
        IMaterialDualBand(t_PartialRange, t_FullRange, t_SolarRadiation)
    {}

    void CMaterialDualBand::createNIRRange(const std::shared_ptr<CMaterial> & t_PartialRange,
                                           const std::shared_ptr<CMaterial> & t_FullRange,
                                           const double t_Fraction)
    {
        auto materials = SingleLayerOptics::createNIRRange(t_PartialRange, t_FullRange, t_Fraction);
        for(auto & material : materials)
        {
            m_Materials.push_back(material);
        }
    }

    void IMaterialDualBand::createRangesFromRatio(double t_Ratio)
    {
        if(!m_Materials.empty())
        {
            return;
        }
        checkIfMaterialWithingSolarRange(*m_MaterialPartialRange);
        createUVRange();
        createNIRRange(m_MaterialPartialRange, m_MaterialFullRange, t_Ratio);

        if(!m_WavelengthsCalculated)
        {
            m_Wavelengths = getWavelengthsFromMaterials();
            m_WavelengthsCalculated = true;
        }
    }

    void IMaterialDualBand::createRangesFromSolarRadiation(
      const FenestrationCommon::CSeries & t_SolarRadiation)
    {
        if(!m_Materials.empty())
        {
            return;
        }
        checkIfMaterialWithingSolarRange(*m_MaterialPartialRange);
        createUVRange();
        const double lowLambda = m_MaterialPartialRange->getMinLambda();
        const double highLambda = m_MaterialPartialRange->getMaxLambda();
        CNIRRatio nirRatio = CNIRRatio(t_SolarRadiation, lowLambda, highLambda);
        createNIRRange(m_MaterialPartialRange, m_MaterialFullRange, NIRRatio);
        if(!m_WavelengthsCalculated)
        {
            m_Wavelengths = getWavelengthsFromMaterials();
            m_WavelengthsCalculated = true;
        }
    }

    std::vector<double> IMaterialDualBand::getWavelengthsFromMaterials() const
    {
        std::vector<double> result;

        if(m_MaterialFullRange != nullptr && m_MaterialPartialRange != nullptr)
        {
            result.emplace_back(m_MaterialFullRange->getMinLambda());
            result.emplace_back(0.32);
            result.emplace_back(m_MaterialPartialRange->getMinLambda());
            result.emplace_back(m_MaterialPartialRange->getMaxLambda());
            result.emplace_back(m_MaterialFullRange->getMaxLambda());
        }

        return result;
    }

    std::shared_ptr<CMaterial>
      IMaterialDualBand::getMaterialFromWavelegth(const double wavelength) const
    {
        std::shared_ptr<CMaterial> result;

        for(const auto & material : m_Materials)
        {
            if(material->isWavelengthInRange(wavelength))
            {
                result = material;
            }
        }

        return result;
    }


    ////////////////////////////////////////////////////////////////////////////////////
    ////   CMaterialSample
    ////////////////////////////////////////////////////////////////////////////////////

    CMaterialSample::CMaterialSample(const std::shared_ptr<CSpectralSample> & t_SpectralSample,
                                     const double t_Thickness,
                                     const MaterialType t_Type,
                                     const double minLambda,
                                     const double maxLambda) :
        CMaterial(minLambda, maxLambda)
    {
        if(t_SpectralSample == nullptr)
        {
            throw std::runtime_error("Cannot create specular material from non-existing sample.");
        }

        m_AngularSample =
          std::make_shared<CAngularSpectralSample>(t_SpectralSample, t_Thickness, t_Type);
    }

    CMaterialSample::CMaterialSample(const std::shared_ptr<CSpectralSample> & t_SpectralSample,
                                     const double t_Thickness,
                                     const MaterialType t_Type,
                                     const WavelengthRange t_Range) :
        CMaterial(t_Range)
    {
        if(t_SpectralSample == nullptr)
        {
            throw std::runtime_error("Cannot create specular material from non-existing sample.");
        }

        m_AngularSample =
          std::make_shared<CAngularSpectralSample>(t_SpectralSample, t_Thickness, t_Type);
    }

    void CMaterialSample::setSourceData(CSeries & t_SourceData)
    {
        m_AngularSample->setSourceData(t_SourceData);
    }

    void CMaterialSample::setDetectorData(FenestrationCommon::CSeries & t_DetectorData)
    {
        m_AngularSample->setDetectorData(t_DetectorData);
    }

    double CMaterialSample::getProperty(const Property t_Property,
                                        const Side t_Side,
                                        const CBeamDirection & t_IncomingDirection,
                                        const CBeamDirection &) const
    {
        assert(m_AngularSample);
        return m_AngularSample->getProperty(
          m_MinLambda, m_MaxLambda, t_Property, t_Side, t_IncomingDirection.theta());
    }

    std::vector<double>
      CMaterialSample::getBandProperties(const Property t_Property,
                                         const Side t_Side,
                                         const CBeamDirection & t_IncomingDirection,
                                         const CBeamDirection &) const
    {
        assert(m_AngularSample);
        return m_AngularSample->getWavelengthsProperty(
          m_MinLambda, m_MaxLambda, t_Property, t_Side, t_IncomingDirection.theta());
    }


    std::vector<double> CMaterialSample::calculateBandWavelengths()
    {
        return m_AngularSample->getBandWavelengths();
    }

    void CMaterialSample::setBandWavelengths(const std::vector<double> & wavelengths)
    {
        CMaterial::setBandWavelengths(wavelengths);
        m_AngularSample->setBandWavelengths(m_Wavelengths);
        m_WavelengthsCalculated = true;
    }

    void CMaterialSample::Flipped(bool flipped)
    {
        m_AngularSample->Flipped(flipped);
    }

    ////////////////////////////////////////////////////////////////////////////////////
    ////   CMaterialPhotovoltaic
    ////////////////////////////////////////////////////////////////////////////////////

    CMaterialPhotovoltaic::CMaterialPhotovoltaic(
      const std::shared_ptr<SpectralAveraging::CPhotovoltaicSample> & t_SpectralSample,
      double t_Thickness,
      FenestrationCommon::MaterialType t_Type,
      double minLambda,
      double maxLambda) :
        CMaterialSample(t_SpectralSample, t_Thickness, t_Type, minLambda, maxLambda),
        m_PVSample(t_SpectralSample)
    {}

    CMaterialPhotovoltaic::CMaterialPhotovoltaic(
      const std::shared_ptr<SpectralAveraging::CPhotovoltaicSample> & t_SpectralSample,
      double t_Thickness,
      FenestrationCommon::MaterialType t_Type,
      FenestrationCommon::WavelengthRange t_Range) :
        CMaterialSample(t_SpectralSample, t_Thickness, t_Type, t_Range),
        m_PVSample(t_SpectralSample)
    {}

    FenestrationCommon::CSeries
      CMaterialPhotovoltaic::jscPrime(FenestrationCommon::Side t_Side) const
    {
        return m_PVSample->jscPrime(t_Side);
    }

    ////////////////////////////////////////////////////////////////////////////////////
    ////   CMaterialMeasured
    ////////////////////////////////////////////////////////////////////////////////////

    CMaterialMeasured::CMaterialMeasured(
      const std::shared_ptr<SpectralAveraging::CAngularMeasurements> & t_AngularMeasurements,
      const double minLambda,
      const double maxLambda) :
        CMaterial(minLambda, maxLambda), m_AngularMeasurements(t_AngularMeasurements)
    {
        if(t_AngularMeasurements == nullptr)
        {
            throw std::runtime_error(
              "Cannot create specular and angular material from non-existing sample.");
        }
    }

    CMaterialMeasured::CMaterialMeasured(
      const std::shared_ptr<SpectralAveraging::CAngularMeasurements> & t_AngularMeasurements,
      const WavelengthRange t_Range) :
        CMaterial(t_Range), m_AngularMeasurements(t_AngularMeasurements)
    {
        if(t_AngularMeasurements == nullptr)
        {
            throw std::runtime_error(
              "Cannot create specular and angular material from non-existing sample.");
        }
    }

    void CMaterialMeasured::setSourceData(CSeries & t_SourceData)
    {
        m_AngularMeasurements->setSourceData(t_SourceData);
    }

    double CMaterialMeasured::getProperty(const Property t_Property,
                                          const Side t_Side,
                                          const CBeamDirection & t_IncomingDirection,
                                          const CBeamDirection &) const
    {
        assert(m_AngularMeasurements);
        std::shared_ptr<CSingleAngularMeasurement> aAngular =
          m_AngularMeasurements->getMeasurements(t_IncomingDirection.theta());
        std::shared_ptr<CSpectralSample> aSample = aAngular->getData();
        return aSample->getProperty(m_MinLambda, m_MaxLambda, t_Property, t_Side);
    }


    std::vector<double>
      CMaterialMeasured::getBandProperties(const Property t_Property,
                                           const Side t_Side,
                                           const CBeamDirection & t_IncomingDirection,
                                           const CBeamDirection &) const
    {
        assert(m_AngularMeasurements);
        std::shared_ptr<CSingleAngularMeasurement> aAngular =
          m_AngularMeasurements->getMeasurements(t_IncomingDirection.theta());
        std::shared_ptr<CSpectralSample> aSample = aAngular->getData();
        auto aProperties = aSample->getWavelengthsProperty(t_Property, t_Side);

        std::vector<double> aValues;

        for(const auto & aProperty : aProperties)
        {
            if(aProperty->x() >= m_MinLambda && aProperty->x() <= m_MaxLambda)
            {
                aValues.push_back(aProperty->value());
            }
        }

        return aValues;
    }

    std::vector<double> CMaterialMeasured::calculateBandWavelengths()
    {
        CSingleAngularMeasurement aAngular = *m_AngularMeasurements->getMeasurements(0.0);
        auto aSample = aAngular.getData();

        return aSample->getWavelengthsFromSample();
    }

    CMaterialSingleBandBSDF::CMaterialSingleBandBSDF(std::vector<std::vector<double>> const & t_Tf,
                                                     std::vector<std::vector<double>> const & t_Tb,
                                                     std::vector<std::vector<double>> const & t_Rf,
                                                     std::vector<std::vector<double>> const & t_Rb,
                                                     CBSDFHemisphere const & t_Hemisphere,
                                                     double minLambda,
                                                     double maxLambda) :
        CMaterial(minLambda, maxLambda), m_Hemisphere(t_Hemisphere)
    {
        validateMatrix(t_Tf, m_Hemisphere);
        validateMatrix(t_Tb, m_Hemisphere);
        validateMatrix(t_Rf, m_Hemisphere);
        validateMatrix(t_Rb, m_Hemisphere);
        m_Property[std::make_pair(FenestrationCommon::Property::T,
                                  FenestrationCommon::Side::Front)] = t_Tf;
        m_Property[std::make_pair(FenestrationCommon::Property::T,
                                  FenestrationCommon::Side::Back)] = t_Tb;
        m_Property[std::make_pair(FenestrationCommon::Property::R,
                                  FenestrationCommon::Side::Front)] = t_Rf;
        m_Property[std::make_pair(FenestrationCommon::Property::R,
                                  FenestrationCommon::Side::Back)] = t_Rb;
    }

    CMaterialSingleBandBSDF::CMaterialSingleBandBSDF(std::vector<std::vector<double>> const & t_Tf,
                                                     std::vector<std::vector<double>> const & t_Tb,
                                                     std::vector<std::vector<double>> const & t_Rf,
                                                     std::vector<std::vector<double>> const & t_Rb,
                                                     CBSDFHemisphere const & t_Hemisphere,
                                                     FenestrationCommon::WavelengthRange t_Range) :
        CMaterial(t_Range), m_Hemisphere(t_Hemisphere)
    {
        validateMatrix(t_Tf, m_Hemisphere);
        validateMatrix(t_Tb, m_Hemisphere);
        validateMatrix(t_Rf, m_Hemisphere);
        validateMatrix(t_Rb, m_Hemisphere);
        m_Property[std::make_pair(FenestrationCommon::Property::T,
                                  FenestrationCommon::Side::Front)] = t_Tf;
        m_Property[std::make_pair(FenestrationCommon::Property::T,
                                  FenestrationCommon::Side::Back)] = t_Tb;
        m_Property[std::make_pair(FenestrationCommon::Property::R,
                                  FenestrationCommon::Side::Front)] = t_Rf;
        m_Property[std::make_pair(FenestrationCommon::Property::R,
                                  FenestrationCommon::Side::Back)] = t_Rb;
    }

    double calcDirectHemispheric(std::vector<std::vector<double>> const & m,
                                 CBSDFHemisphere const & hemisphere,
                                 size_t incomingIdx)
    {
        const auto outgoingLambdas =
          hemisphere.getDirections(BSDFDirection::Outgoing).lambdaVector();

        double result = 0;
        for(size_t outgoingIdx = 0; outgoingIdx < outgoingLambdas.size(); ++outgoingIdx)
        {
            result += m[outgoingIdx][incomingIdx] * outgoingLambdas[outgoingIdx];
        }
        return result;
    }


    double CMaterialSingleBandBSDF::getProperty(FenestrationCommon::Property t_Property,
                                                FenestrationCommon::Side t_Side,
                                                const CBeamDirection & t_IncomingDirection,
                                                const CBeamDirection & t_OutgoingDirection) const
    {
        const auto incomingIdx =
          m_Hemisphere.getDirections(BSDFDirection::Incoming)
            .getNearestBeamIndex(t_IncomingDirection.theta(), t_IncomingDirection.phi());

        if(t_Property == FenestrationCommon::Property::Abs)
        {
            double tHem = calcDirectHemispheric(
              m_Property.at({FenestrationCommon::Property::T, t_Side}), m_Hemisphere, incomingIdx);
            double rHem = calcDirectHemispheric(
              m_Property.at({FenestrationCommon::Property::R, t_Side}), m_Hemisphere, incomingIdx);
            return 1 - tHem - rHem;
        }
        else
        {
            const auto outgoingIdx =
              m_Hemisphere.getDirections(BSDFDirection::Outgoing)
                .getNearestBeamIndex(t_OutgoingDirection.theta(), t_OutgoingDirection.phi());

            auto lambda{m_Hemisphere.getDirections(BSDFDirection::Outgoing).lambdaVector()};

            const auto val = m_Property.at({t_Property, t_Side})[outgoingIdx][incomingIdx];

            return val * lambda[outgoingIdx];
        }
    }

    std::vector<double>
      CMaterialSingleBandBSDF::getBandProperties(FenestrationCommon::Property t_Property,
                                                 FenestrationCommon::Side t_Side,
                                                 const CBeamDirection & t_IncomingDirection,
                                                 const CBeamDirection & t_OutgoingDirection) const
    {
        double value = getProperty(t_Property, t_Side, t_IncomingDirection, t_OutgoingDirection);
        std::vector<double> bandProperties{value, value};
        return bandProperties;
    }

    std::vector<std::vector<double>> const &
      CMaterialSingleBandBSDF::getBSDFMatrix(FenestrationCommon::Property const & t_Property,
                                             FenestrationCommon::Side const & t_Side) const
    {
        return m_Property.at({t_Property, t_Side});
    }

    CBSDFHemisphere CMaterialSingleBandBSDF::getHemisphere() const
    {
        return m_Hemisphere;
    }

    std::vector<double> CMaterialSingleBandBSDF::calculateBandWavelengths()
    {
        std::vector<double> aWavelengths;
        aWavelengths.push_back(m_MinLambda);
        aWavelengths.push_back(m_MaxLambda);
        return aWavelengths;
    }

    void CMaterialSingleBandBSDF::validateMatrix(std::vector<std::vector<double>> const & matrix,
                                                 CBSDFHemisphere const & hemisphere) const
    {
        size_t rowCt = matrix.size();
        size_t colCt = matrix[0].size();
        size_t hemisphereIncomingCt = hemisphere.getDirections(BSDFDirection::Incoming).size();
        size_t hemisphereOutgoingCt = hemisphere.getDirections(BSDFDirection::Outgoing).size();

        if(rowCt != hemisphereIncomingCt)
        {
            std::stringstream msg;
            msg << "Incompatible number of incoming directions.  BSDF matrix: << " << rowCt
                << " BSDF Hemispere: " << hemisphereIncomingCt;
            throw std::runtime_error(msg.str());
        }

        if(colCt != hemisphereOutgoingCt)
        {
            std::stringstream msg;
            msg << "Incompatible number of incoming directions.  BSDF matrix: << " << colCt
                << " BSDF Hemispere: " << hemisphereOutgoingCt;
            throw std::runtime_error(msg.str());
        }
    }

    CMaterialDualBandBSDF::CMaterialDualBandBSDF(
      const std::shared_ptr<CMaterialSingleBandBSDF> & t_PartialRange,
      const std::shared_ptr<CMaterialSingleBandBSDF> & t_FullRange,
      double t_Ratio) :
        IMaterialDualBand(t_PartialRange, t_FullRange, t_Ratio)
    {}

    CMaterialDualBandBSDF::CMaterialDualBandBSDF(
      const std::shared_ptr<CMaterialSingleBandBSDF> & t_PartialRange,
      const std::shared_ptr<CMaterialSingleBandBSDF> & t_FullRange,
      const FenestrationCommon::CSeries & t_SolarRadiation) :
        IMaterialDualBand(t_PartialRange, t_FullRange, t_SolarRadiation)
    {}

    void CMaterialDualBandBSDF::createNIRRange(const std::shared_ptr<CMaterial> & t_PartialRange,
                                               const std::shared_ptr<CMaterial> & t_FullRange,
                                               const double t_Fraction)
    {
        auto materials = SingleLayerOptics::createNIRRange(
          std::dynamic_pointer_cast<CMaterialSingleBandBSDF>(t_PartialRange),
          std::dynamic_pointer_cast<CMaterialSingleBandBSDF>(t_FullRange),
          t_Fraction);
        for(auto & material : materials)
        {
            m_Materials.push_back(material);
        }
    }

}   // namespace SingleLayerOptics
