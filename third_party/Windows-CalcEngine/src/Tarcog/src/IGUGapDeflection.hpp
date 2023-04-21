#ifndef TARIGUGAPLAYERDEFLECTION_H
#define TARIGUGAPLAYERDEFLECTION_H

#include <memory>

#include "IGUGapLayer.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        class CIGUGapLayerDeflection : public CIGUGapLayer
        {
        public:
            CIGUGapLayerDeflection(const CIGUGapLayer & t_GapLayer, double t_Tini, double t_Pini);

            double getPressure() override;

            std::shared_ptr<CBaseLayer> clone() const override;

        private:
            // Windows is produced under given temperature and pressure.
            // That is used for deflection calculations.
            double m_Tini;
            double m_Pini;
        };

    }   // namespace ISO15099

}   // namespace Tarcog

#endif
