#ifndef TARCOGCONSTANTS_H
#define TARCOGCONSTANTS_H

namespace Tarcog
{
    namespace TarcogConstants
    {
        const double DEFAULT_WINDOW_WIDTH = 1;
        const double DEFAULT_WINDOW_HEIGHT = 1;
        const double DEFAULT_TILT = 90;
        const double DEFAULT_ENV_EMISSIVITY = 1;
        const double DEFAULT_FRACTION_OF_CLEAR_SKY = 1;
    }   // namespace TarcogConstants

    namespace IterationConstants
    {
        const double RELAXATION_PARAMETER_MAX = 0.65;
        const double RELAXATION_PARAMETER_MIN = 0.05;
        const double RELAXATION_PARAMETER_STEP = 0.05;
        const double CONVERGENCE_TOLERANCE = 1e-6;
        const size_t NUMBER_OF_STEPS = 200;
        const double RELAXATION_PARAMETER_AIRFLOW = 0.9;
        const double RELAXATION_PARAMETER_AIRFLOW_MIN = 0.1;
        const double RELAXATION_PARAMETER_AIRFLOW_STEP = 0.1;
        const double CONVERGENCE_TOLERANCE_AIRFLOW = 1e-6;
    }   // namespace IterationConstants

    namespace DeflectionConstants
    {
        const double YOUNGSMODULUS = 7.2e10;
        const double POISONRATIO = 0.22;   // This constant is for glass
    }                                      // namespace DeflectionConstants

}   // namespace Tarcog


#endif
