#pragma once

namespace Tarcog
{
    namespace ISO15099
    {
        enum class BoundaryConditionsCoeffModel
        {
            CalculateH,
            HPrescribed,
            HcPrescribed
        };

        enum class Environment
        {
            Indoor,
            Outdoor
        };
    }   // namespace ISO15099
}   // namespace Tarcog
