#ifndef BSDFPHILIMITS_H
#define BSDFPHILIMITS_H

#include <vector>
#include <memory>

namespace SingleLayerOptics
{
    class CPhiLimits
    {
    public:
        explicit CPhiLimits(size_t t_NumOfPhis);

        const std::vector<double> & getPhiLimits() const;

    private:
        void createLimits(const std::vector<double> & t_PhiAngles);
        std::vector<double> m_PhiLimits;
    };

}   // namespace SingleLayerOptics

#endif
