#include "Environments.hpp"
#include "OutdoorEnvironment.hpp"
#include "IndoorEnvironment.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        std::shared_ptr<CIndoorEnvironment>
          Tarcog::ISO15099::Environments::indoor(double roomAirTemperature, double roomPressure)
        {
            return std::make_shared<CIndoorEnvironment>(roomAirTemperature, roomPressure);
        }

        std::shared_ptr<COutdoorEnvironment>
          Environments::outdoor(double airTemperature,
                                double airSpeed,
                                double solarRadiation,
                                double skyTemperature,
                                SkyModel skyModel,
                                double pressure,
                                AirHorizontalDirection airDirection,
                                double fractionOfClearSky)
        {
            return std::make_shared<COutdoorEnvironment>(airTemperature,
                                                         airSpeed,
                                                         solarRadiation,
                                                         airDirection,
                                                         skyTemperature,
                                                         skyModel,
                                                         pressure,
                                                         fractionOfClearSky);
        }
    }   // namespace ISO15099
}   // namespace Tarcog