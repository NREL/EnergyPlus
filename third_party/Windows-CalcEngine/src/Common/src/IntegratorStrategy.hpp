#ifndef INTEGRATIONSTRATEGY_H
#define INTEGRATIONSTRATEGY_H

#include <memory>
#include <vector>
#include "Series.hpp"


namespace FenestrationCommon
{
    enum class IntegrationType
    {
        Rectangular,
        RectangularCentroid,
        Trapezoidal,
        TrapezoidalA,
        TrapezoidalB,
        PreWeighted
    };

    class ISeriesPoint;

    class CSeries;

    class IIntegratorStrategy
    {
    public:
        virtual ~IIntegratorStrategy() = default;

        // virtual double integrate( double const x1, double const x2, double const
        // y1, double const y2 ) = 0;
        virtual std::unique_ptr<CSeries>
          integrate(const std::vector<std::unique_ptr<ISeriesPoint>> & t_Series,
                    double normalizationCoeff = 1) = 0;

    protected:
        double dX(double x1, double x2) const;
    };

    class CIntegratorRectangular : public IIntegratorStrategy
    {
    public:
        std::unique_ptr<CSeries>
          integrate(const std::vector<std::unique_ptr<ISeriesPoint>> & t_Series,
                    double normalizationCoeff) override;
    };

    class CIntegratorRectangularCentroid : public IIntegratorStrategy
    {
    public:
        std::unique_ptr<CSeries>
          integrate(const std::vector<std::unique_ptr<ISeriesPoint>> & t_Series,
                    double normalizationCoeff) override;
    };

    class CIntegratorTrapezoidal : public IIntegratorStrategy
    {
    public:
        std::unique_ptr<CSeries>
          integrate(const std::vector<std::unique_ptr<ISeriesPoint>> & t_Series,
                    double normalizationCoeff) override;
    };

    class CIntegratorTrapezoidalA : public IIntegratorStrategy
    {
    public:
        std::unique_ptr<CSeries>
          integrate(const std::vector<std::unique_ptr<ISeriesPoint>> & t_Series,
                    double normalizationCoeff) override;
    };

    class CIntegratorTrapezoidalB : public IIntegratorStrategy
    {
    public:
        std::unique_ptr<CSeries>
          integrate(const std::vector<std::unique_ptr<ISeriesPoint>> & t_Series,
                    double normalizationCoeff) override;
    };

    class CIntegratorPreWeighted : public IIntegratorStrategy
    {
    public:
        std::unique_ptr<CSeries>
          integrate(const std::vector<std::unique_ptr<ISeriesPoint>> & t_Series,
                    double normalizationCoeff) override;
    };

    class CIntegratorFactory
    {
    public:
        std::unique_ptr<IIntegratorStrategy> getIntegrator(IntegrationType t_IntegratorType) const;
    };

}   // namespace FenestrationCommon

#endif
