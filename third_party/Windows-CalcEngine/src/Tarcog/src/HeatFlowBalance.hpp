#ifndef TARCOGQBALANCE_H
#define TARCOGQBALANCE_H

#include <memory>
#include <vector>
#include "WCECommon.hpp"
#include "IGU.hpp"

namespace FenestrationCommon
{
    class SquareMatrix;
    class CLinearSolver;

}   // namespace FenestrationCommon

namespace Tarcog
{
    namespace ISO15099
    {
        class CBaseLayer;

        class CHeatFlowBalance
        {
        public:
            explicit CHeatFlowBalance(CIGU & t_IGU);

            std::vector<double> calcBalanceMatrix();

        private:
            void buildCell(Tarcog::ISO15099::CBaseLayer & t_Current, size_t t_Index);

            FenestrationCommon::SquareMatrix m_MatrixA;
            std::vector<double> m_VectorB;

            CIGU & m_IGU;
        };

    }   // namespace ISO15099

}   // namespace Tarcog

#endif
