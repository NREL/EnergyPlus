#include <stdexcept>

#include "ScatteringLayer.hpp"
#include "LayerSingleComponent.hpp"
#include "OpticalSurface.hpp"
#include "BaseCell.hpp"
#include "BSDFLayer.hpp"
#include "MaterialDescription.hpp"
#include "BSDFDirections.hpp"
#include "BSDFIntegrator.hpp"
#include "BeamDirection.hpp"
#include "WCESpectralAveraging.hpp"
#include "WCECommon.hpp"
#include "SpecularBSDFLayer.hpp"

using namespace SingleLayerOptics;
using namespace FenestrationCommon;

namespace SingleLayerOptics
{
    std::map<EmissivityPolynomials, std::vector<double>> emissPolynomial{
      {EmissivityPolynomials::NFRC_301_Coated, {1.3217, -1.8766, 4.6586, -5.8349, 2.7406}},
      {EmissivityPolynomials::NFRC_301_Uncoated, {0.1569, 3.7669, -5.4398, 2.4733}},
      {EmissivityPolynomials::EN12898, {1.1887, -0.4967, 0.2452}}};

    CScatteringLayer::CScatteringLayer(const CScatteringSurface & t_Front,
                                       const CScatteringSurface & t_Back) :
        m_Surface({{Side::Front, t_Front}, {Side::Back, t_Back}}),
        m_BSDFLayer(nullptr),
        m_Theta(0),
        m_Phi(0)
    {}

    CScatteringLayer::CScatteringLayer(CScatteringSurface && t_Front,
                                       CScatteringSurface && t_Back) :
        m_Surface({{Side::Front, std::move(t_Front)}, {Side::Back, std::move(t_Back)}}),
        m_BSDFLayer(nullptr),
        m_Theta(0),
        m_Phi(0)
    {}

    CScatteringLayer::CScatteringLayer(const double Tf_dir_dir,
                                       const double Rf_dir_dir,
                                       const double Tb_dir_dir,
                                       const double Rb_dir_dir,
                                       const double Tf_dir_dif,
                                       const double Rf_dir_dif,
                                       const double Tb_dir_dif,
                                       const double Rb_dir_dif,
                                       const double Tf_dif_dif,
                                       const double Rf_dif_dif,
                                       const double Tb_dif_dif,
                                       const double Rb_dif_dif) :
        m_Surface({{Side::Front,
                    CScatteringSurface(
                      Tf_dir_dir, Rf_dir_dir, Tf_dir_dif, Rf_dir_dif, Tf_dif_dif, Rf_dif_dif)},
                   {Side::Back,
                    CScatteringSurface(
                      Tb_dir_dir, Rb_dir_dir, Tb_dir_dif, Rb_dir_dif, Tb_dif_dif, Rb_dif_dif)}}),
        m_BSDFLayer(nullptr),
        m_Theta(0),
        m_Phi(0)
    {}

    CScatteringLayer::CScatteringLayer(const std::shared_ptr<CMaterial> & t_Material,
                                       std::shared_ptr<ICellDescription> t_Description,
                                       const DistributionMethod t_Method) :
        m_BSDFLayer(nullptr), m_Theta(0), m_Phi(0)
    {
        // Scattering layer can also be created from material and cell desctiption in which case
        // integration will be performed using BSDF distribution while direct-direct component will
        // be taken directly from cell.
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Full);
        auto aMaker = CBSDFLayerMaker(t_Material, aBSDF, t_Description, t_Method);
        m_BSDFLayer = aMaker.getLayer();
    }

    void CScatteringLayer::setSourceData(CSeries & t_SourceData) const
    {
        if(m_BSDFLayer != nullptr)
        {
            m_BSDFLayer->setSourceData(t_SourceData);
        }
    }

    void CScatteringLayer::setBlackBodySource(const double temperature)
    {
        // This will load full wavelengths data from sample. However, only wavelength higher
        // than 5.0 are needed in black body source integration.
        auto wlFull = getWavelengths();
        std::vector<double> wl;
        for(const auto wwl : wlFull)
        {
            if(wwl >= 5.0)
            {
                wl.push_back(wwl);
            }
        }
        const auto spectrum = SpectralAveraging::BlackBodySpectrum(wl, temperature);
        CSeries seriesSpectrum{spectrum};
        setSourceData(seriesSpectrum);
        setWavelengths(wl);
    }

    CScatteringSurface & CScatteringLayer::getSurface(const Side t_Side)
    {
        if(m_Surface.empty())
        {
            m_Theta = 0;
            m_Phi = 0;
            createResultsAtAngle(m_Theta, m_Phi);
        }
        return m_Surface.at(t_Side);
    }

    double CScatteringLayer::getPropertySimple(const double,
                                               const double,
                                               const PropertySimple t_Property,
                                               const Side t_Side,
                                               const Scattering t_Scattering,
                                               const double t_Theta,
                                               const double t_Phi)
    {
        checkCurrentAngles(t_Theta, t_Phi);
        auto aSurface = getSurface(t_Side);
        return aSurface.getPropertySimple(t_Property, t_Scattering);
    }

    double CScatteringLayer::getAbsorptance(const Side t_Side,
                                            const ScatteringSimple t_Scattering,
                                            const double t_Theta,
                                            const double t_Phi)
    {
        checkCurrentAngles(t_Theta, t_Phi);
        auto & aSurface = getSurface(t_Side);
        return aSurface.getAbsorptance(t_Scattering);
    }

    double
      CScatteringLayer::getAbsorptance(const Side t_Side, const double t_Theta, const double t_Phi)
    {
        checkCurrentAngles(t_Theta, t_Phi);
        auto aSurface = getSurface(t_Side);
        return aSurface.getAbsorptance();
    }

    std::vector<double> CScatteringLayer::getAbsorptanceLayers(const double,
                                                               const double,
                                                               FenestrationCommon::Side side,
                                                               FenestrationCommon::ScatteringSimple,
                                                               const double theta,
                                                               const double phi)
    {
        std::vector<double> abs;
        abs.push_back(getAbsorptance(side, theta, phi));
        return abs;
    }

    CLayerSingleComponent CScatteringLayer::getLayer(const Scattering t_Scattering,
                                                     const double t_Theta,
                                                     const double t_Phi)
    {
        double Tf = getPropertySimple(getMinLambda(),
                                      getMaxLambda(),
                                      PropertySimple::T,
                                      Side::Front,
                                      t_Scattering,
                                      t_Theta,
                                      t_Phi);
        double Rf = getPropertySimple(getMinLambda(),
                                      getMaxLambda(),
                                      PropertySimple::R,
                                      Side::Front,
                                      t_Scattering,
                                      t_Theta,
                                      t_Phi);
        double Tb = getPropertySimple(getMinLambda(),
                                      getMaxLambda(),
                                      PropertySimple::T,
                                      Side::Back,
                                      t_Scattering,
                                      t_Theta,
                                      t_Phi);
        double Rb = getPropertySimple(getMinLambda(),
                                      getMaxLambda(),
                                      PropertySimple::R,
                                      Side::Back,
                                      t_Scattering,
                                      t_Theta,
                                      t_Phi);
        return CLayerSingleComponent(Tf, Rf, Tb, Rb);
    }

    std::vector<double> CScatteringLayer::getWavelengths() const
    {
        return m_BSDFLayer->getBandWavelengths();
    }

    void CScatteringLayer::setWavelengths(const std::vector<double> & wavelengths)
    {
        m_BSDFLayer->setBandWavelengths(wavelengths);
    }

    void CScatteringLayer::createResultsAtAngle(const double t_Theta, const double t_Phi)
    {
        if(m_BSDFLayer != nullptr)
        {
            m_Surface.clear();
            m_Surface.emplace(Side::Front, createSurface(Side::Front, t_Theta, t_Phi));
            m_Surface.emplace(Side::Back, createSurface(Side::Back, t_Theta, t_Phi));
        }
    }

    CScatteringSurface
      CScatteringLayer::createSurface(const Side t_Side, const double t_Theta, const double t_Phi)
    {
        CBeamDirection aDirection = CBeamDirection(t_Theta, t_Phi);
        double T_dir_dir = m_BSDFLayer->getCell()->T_dir_dir(t_Side, aDirection);
        double R_dir_dir = m_BSDFLayer->getCell()->R_dir_dir(t_Side, aDirection);
        double T_dir_dif =
          m_BSDFLayer->getResults()->DirHem(t_Side, PropertySimple::T, t_Theta, t_Phi) - T_dir_dir;
        if(T_dir_dif < 0)
        {
            T_dir_dif = 0;
        }
        double R_dir_dif =
          m_BSDFLayer->getResults()->DirHem(t_Side, PropertySimple::R, t_Theta, t_Phi) - R_dir_dir;
        if(R_dir_dif < 0)
        {
            R_dir_dif = 0;
        }
        double T_dif_dif = m_BSDFLayer->getResults()->DiffDiff(t_Side, PropertySimple::T);
        double R_dif_dif = m_BSDFLayer->getResults()->DiffDiff(t_Side, PropertySimple::R);
        return CScatteringSurface(T_dir_dir, R_dir_dir, T_dir_dif, R_dir_dif, T_dif_dif, R_dif_dif);
    }

    bool CScatteringLayer::checkCurrentAngles(const double t_Theta, const double t_Phi)
    {
        bool curAngles = ((t_Theta == m_Theta) && (t_Phi == m_Phi));
        if(!curAngles)
        {
            m_Theta = t_Theta;
            m_Phi = t_Phi;
            createResultsAtAngle(m_Theta, m_Phi);
        }
        return false;
    }

    double CScatteringLayer::getMinLambda() const
    {
        double result{0};
        if(m_BSDFLayer != nullptr)
        {
            result = m_BSDFLayer->getCell()->getMinLambda();
        }
        return result;
    }

    double CScatteringLayer::getMaxLambda() const
    {
        double result{0};
        if(m_BSDFLayer != nullptr)
        {
            result = m_BSDFLayer->getCell()->getMaxLambda();
        }
        return result;
    }

    CScatteringLayer
      CScatteringLayer::createSpecularLayer(const std::shared_ptr<CMaterial> & t_Material)
    {
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Full);
        return CScatteringLayer(CBSDFLayerMaker::getSpecularLayer(t_Material, aBSDF));
    }

    CScatteringLayer::CScatteringLayer(const std::shared_ptr<CBSDFLayer> & aBSDF) :
        m_BSDFLayer(aBSDF)
    {}

    CScatteringLayer CScatteringLayer::createWovenLayer(
      const std::shared_ptr<CMaterial> & t_Material, const double diameter, const double spacing)
    {
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Full);
        return CScatteringLayer(
          CBSDFLayerMaker::getWovenLayer(t_Material, aBSDF, diameter, spacing));
    }

    CScatteringLayer
      CScatteringLayer::createPerfectlyDiffusingLayer(const std::shared_ptr<CMaterial> & t_Material)
    {
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Full);
        return CScatteringLayer(CBSDFLayerMaker::getPerfectlyDiffuseLayer(t_Material, aBSDF));
    }

    CScatteringLayer
      CScatteringLayer::createVenetianLayer(const std::shared_ptr<CMaterial> & t_Material,
                                            const double slatWidth,
                                            const double slatSpacing,
                                            const double slatTiltAngle,
                                            const double curvatureRadius,
                                            const size_t numOfSlatSegments,
                                            const DistributionMethod method,
                                            const bool isHorizontal)
    {
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Full);
        return CScatteringLayer(CBSDFLayerMaker::getVenetianLayer(t_Material,
                                                                  aBSDF,
                                                                  slatWidth,
                                                                  slatSpacing,
                                                                  slatTiltAngle,
                                                                  curvatureRadius,
                                                                  numOfSlatSegments,
                                                                  method,
                                                                  isHorizontal));
    }

    CScatteringLayer
      CScatteringLayer::createPerforatedCircularLayer(const std::shared_ptr<CMaterial> & t_Material,
                                                      double x,
                                                      double y,
                                                      double thickness,
                                                      double radius)
    {
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Full);
        return CScatteringLayer(
          CBSDFLayerMaker::getCircularPerforatedLayer(t_Material, aBSDF, x, y, thickness, radius));
    }

    CScatteringLayer CScatteringLayer::createPerforatedRectangularLayer(
      const std::shared_ptr<CMaterial> & t_Material,
      const double x,
      const double y,
      const double thickness,
      const double xHole,
      const double yHole)
    {
        const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Full);
        return CScatteringLayer(CBSDFLayerMaker::getRectangularPerforatedLayer(
          t_Material, aBSDF, x, y, thickness, xHole, yHole));
    }

    bool CScatteringLayer::canApplyEmissivityPolynomial() const
    {
        return m_BSDFLayer != nullptr
               && std::dynamic_pointer_cast<CSpecularBSDFLayer>(m_BSDFLayer) != nullptr
               && m_BSDFLayer->getBandWavelengths().size() > 2;
    }

    ////////////////////////////////////////////////////////////////////////////////////
    /// CScatteringLayerIR
    ////////////////////////////////////////////////////////////////////////////////////

    CScatteringLayerIR::CScatteringLayerIR(CScatteringLayer layer) : m_Layer(std::move(layer))
    {}

    double CScatteringLayerIR::emissivity(Side t_Side, EmissivityPolynomials type)
    {
        return emissivity(t_Side, emissPolynomial.at(type));
    }

    double CScatteringLayerIR::emissivity(Side t_Side, const std::vector<double> & polynomial)
    {
        double value = 0;
        if(m_Layer.canApplyEmissivityPolynomial())
        {
            double abs = m_Layer.getAbsorptance(t_Side, ScatteringSimple::Direct, 0, 0);
            for(size_t i = 0; i < polynomial.size(); ++i)
            {
                value += std::pow(abs, i + 1) * polynomial[i];
            }
        }
        else
        {
            value = m_Layer.getAbsorptance(t_Side, ScatteringSimple::Diffuse, 0, 0);
        }
        return value;
    }

    double CScatteringLayerIR::transmittance(Side t_Side)
    {
        CWavelengthRange wrIR{WavelengthRange::IR};
        return m_Layer.getPropertySimple(wrIR.minLambda(),
                                         wrIR.maxLambda(),
                                         PropertySimple::T,
                                         t_Side,
                                         Scattering::DiffuseDiffuse);
    }

}   // namespace SingleLayerOptics
