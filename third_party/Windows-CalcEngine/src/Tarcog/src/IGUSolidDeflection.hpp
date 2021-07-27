#ifndef TARIGUSOLIDLAYERDEFLECTION_H
#define TARIGUSOLIDLAYERDEFLECTION_H

#include <memory>

#include "IGUSolidLayer.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        ////////////////////////////////////////////////////////////////////////////
        ////    CIGUSolidLayerDeflection
        ////////////////////////////////////////////////////////////////////////////
        class CIGUSolidLayerDeflection : public CIGUSolidLayer
        {
        public:
            explicit CIGUSolidLayerDeflection(const CIGUSolidLayer & t_SolidLayer);
            CIGUSolidLayerDeflection(const CIGUSolidLayer & t_SolidLayer,
                                     double t_YoungsModulus,
                                     double t_PoisonRatio);

            double flexuralRigidity() const;
            bool isDeflected() const override;

            std::shared_ptr<CBaseLayer> clone() const override;

        protected:
            void calculateConvectionOrConductionFlow() override;
            double pressureDifference() const;

        private:
            double m_YoungsModulus;
            double m_PoisonRatio;
        };

        ////////////////////////////////////////////////////////////////////////////
        ////    CIGUDeflectionTempAndPressure
        ////////////////////////////////////////////////////////////////////////////
        class CIGUDeflectionTempAndPressure : public CIGUSolidLayerDeflection
        {
        public:
            CIGUDeflectionTempAndPressure(
              const std::shared_ptr<CIGUSolidLayerDeflection> & t_SolidLayer,
              double t_MaxDeflectionCoeff,
              double t_MeanDeflectionCoeff);

            std::shared_ptr<CBaseLayer> clone() const override;

        protected:
            void calculateConvectionOrConductionFlow() override;

        private:
            double LdMean(double t_P, double t_D) const;
            double LdMax(double t_P, double t_D) const;

            double m_MaxCoeff;
            double m_MeanCoeff;
        };

        ////////////////////////////////////////////////////////////////////////////
        ////    CIGUDeflectionMeasuread
        ////////////////////////////////////////////////////////////////////////////
        class CIGUDeflectionMeasuread : public CIGUSolidLayerDeflection
        {
        public:
            CIGUDeflectionMeasuread(std::shared_ptr<CIGUSolidLayerDeflection> & t_Layer,
                                    const double t_MeanDeflection,
                                    const double t_MaxDeflection);
        };
    }   // namespace ISO15099
}   // namespace Tarcog

#endif
