#include "BSDFIntegrator.hpp"
#include "BSDFDirections.hpp"
#include "BSDFPatch.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;

namespace SingleLayerOptics
{
    CBSDFIntegrator::CBSDFIntegrator(const std::shared_ptr<const CBSDFIntegrator> & t_Integrator) :
        m_Directions(t_Integrator->m_Directions),
        m_DimMatrices(m_Directions.size()),
        m_HemisphericalCalculated(false),
        m_DiffuseDiffuseCalculated(false)
    {
        for(auto t_Side : EnumSide())
        {
            for(auto t_Property : EnumPropertySimple())
            {
                m_Matrix[std::make_pair(t_Side, t_Property)] = SquareMatrix(m_DimMatrices);
                m_Hem[std::make_pair(t_Side, t_Property)] = std::vector<double>(m_DimMatrices);
            }
        }
    }

    CBSDFIntegrator::CBSDFIntegrator(const CBSDFDirections & t_Directions) :
        m_Directions(t_Directions),
        m_DimMatrices(m_Directions.size()),
        m_HemisphericalCalculated(false),
        m_DiffuseDiffuseCalculated(false)
    {
        for(auto t_Side : EnumSide())
        {
            for(auto t_Property : EnumPropertySimple())
            {
                m_Matrix[std::make_pair(t_Side, t_Property)] = SquareMatrix(m_DimMatrices);
                m_Hem[std::make_pair(t_Side, t_Property)] = std::vector<double>(m_DimMatrices);
            }
        }
    }

    double CBSDFIntegrator::DiffDiff(const Side t_Side, const PropertySimple t_Property)
    {
        calcDiffuseDiffuse();
        return m_MapDiffDiff.at(t_Side, t_Property);
    }

    SquareMatrix & CBSDFIntegrator::getMatrix(const Side t_Side, const PropertySimple t_Property)
    {
        return m_Matrix[std::make_pair(t_Side, t_Property)];
    }

    const FenestrationCommon::SquareMatrix &
      CBSDFIntegrator::at(const FenestrationCommon::Side t_Side,
                          const FenestrationCommon::PropertySimple t_Property) const
    {
        return m_Matrix.at(std::make_pair(t_Side, t_Property));
    }

    void CBSDFIntegrator::setResultMatrices(const SquareMatrix & t_Tau,
                                            const SquareMatrix & t_Rho,
                                            Side t_Side)
    {
        m_Matrix[std::make_pair(t_Side, PropertySimple::T)] = t_Tau;
        m_Matrix[std::make_pair(t_Side, PropertySimple::R)] = t_Rho;
    }

    double CBSDFIntegrator::DirDir(const Side t_Side,
                                   const PropertySimple t_Property,
                                   const double t_Theta,
                                   const double t_Phi) const
    {
        const auto index = m_Directions.getNearestBeamIndex(t_Theta, t_Phi);
        const auto lambda = m_Directions.lambdaVector()[index];
        const auto tau = at(t_Side, t_Property)(index, index);
        return tau * lambda;
    }

    double CBSDFIntegrator::DirDir(const Side t_Side,
                                   const PropertySimple t_Property,
                                   const size_t Index) const
    {
        const auto lambda = m_Directions.lambdaVector()[Index];
        const auto tau = at(t_Side, t_Property)(Index, Index);
        return tau * lambda;
    }

    std::vector<double> CBSDFIntegrator::DirHem(const FenestrationCommon::Side t_Side,
                                                const FenestrationCommon::PropertySimple t_Property)
    {
        calcHemispherical();
        return m_Hem.at(std::make_pair(t_Side, t_Property));
    }

    std::vector<double> CBSDFIntegrator::Abs(Side t_Side)
    {
        calcHemispherical();
        return m_Abs.at(t_Side);
    }

    double CBSDFIntegrator::DirHem(const Side t_Side,
                                   const PropertySimple t_Property,
                                   const double t_Theta,
                                   const double t_Phi)
    {
        const auto index = m_Directions.getNearestBeamIndex(t_Theta, t_Phi);
        return DirHem(t_Side, t_Property)[index];
    }

    double CBSDFIntegrator::Abs(const Side t_Side, const double t_Theta, const double t_Phi)
    {
        const auto index = m_Directions.getNearestBeamIndex(t_Theta, t_Phi);
        return Abs(t_Side)[index];
    }

    double CBSDFIntegrator::Abs(const Side t_Side, const size_t Index)
    {
        return Abs(t_Side)[Index];
    }

    std::vector<double> CBSDFIntegrator::lambdaVector() const
    {
        return m_Directions.lambdaVector();
    }

    SquareMatrix CBSDFIntegrator::lambdaMatrix() const
    {
        return m_Directions.lambdaMatrix();
    }

    double CBSDFIntegrator::integrate(SquareMatrix const & t_Matrix) const
    {
        using ConstantsData::WCE_PI;
        double sum = 0;
        for(size_t i = 0; i < m_DimMatrices; ++i)
        {
            for(size_t j = 0; j < m_DimMatrices; ++j)
            {
                sum += t_Matrix(i, j) * m_Directions[i].lambda() * m_Directions[j].lambda();
            }
        }
        return sum / WCE_PI;
    }

    void CBSDFIntegrator::calcDiffuseDiffuse()
    {
        if(!m_DiffuseDiffuseCalculated)
        {
            for(auto t_Side : EnumSide())
            {
                for(auto t_Property : EnumPropertySimple())
                {
                    m_MapDiffDiff(t_Side, t_Property) = integrate(getMatrix(t_Side, t_Property));
                }
            }
            m_DiffuseDiffuseCalculated = true;
        }
    }

    size_t CBSDFIntegrator::getNearestBeamIndex(const double t_Theta, const double t_Phi) const
    {
        return m_Directions.getNearestBeamIndex(t_Theta, t_Phi);
    }

    void CBSDFIntegrator::calcHemispherical()
    {
        if(!m_HemisphericalCalculated)
        {
            for(Side t_Side : EnumSide())
            {
                for(PropertySimple t_Property : EnumPropertySimple())
                {
                    m_Hem[std::make_pair(t_Side, t_Property)] =
                      m_Directions.lambdaVector() * m_Matrix.at(std::make_pair(t_Side, t_Property));
                }
                m_Abs[t_Side] = std::vector<double>();
            }

            const auto size = m_Hem[std::make_pair(Side::Front, PropertySimple::T)].size();
            for(size_t i = 0; i < size; ++i)
            {
                for(Side t_Side : EnumSide())
                {
                    m_Abs.at(t_Side).push_back(
                      1.0 - m_Hem.at(std::make_pair(t_Side, PropertySimple::T))[i]
                      - m_Hem.at(std::make_pair(t_Side, PropertySimple::R))[i]);
                }
            }
            m_HemisphericalCalculated = true;
        }
    }
    double CBSDFIntegrator::AbsDiffDiff(FenestrationCommon::Side t_Side)
    {
        return 1 - DiffDiff(t_Side, PropertySimple::T) - DiffDiff(t_Side, PropertySimple::R);
    }

}   // namespace SingleLayerOptics
