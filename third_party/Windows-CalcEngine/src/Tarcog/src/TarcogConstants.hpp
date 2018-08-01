#ifndef TARCOGCONSTANTS_H
#define TARCOGCONSTANTS_H

namespace Tarcog {

	namespace TarcogConstants {
		double const DEFAULT_WINDOW_WIDTH = 1;
		double const DEFAULT_WINDOW_HEIGHT = 1;
		double const DEFAULT_TILT = 90;
		double const DEFAULT_ENV_EMISSIVITY = 1;
		double const DEFAULT_FRACTION_OF_CLEAR_SKY = 1;
	}

	namespace IterationConstants {
		double const RELAXATION_PARAMETER_MAX = 0.65;
		double const RELAXATION_PARAMETER_MIN = 0.05;
		double const RELAXATION_PARAMETER_STEP = 0.05;
		double const CONVERGENCE_TOLERANCE = 1e-6;
		size_t const NUMBER_OF_STEPS = 200;
		double const RELAXATION_PARAMETER_AIRFLOW = 0.9;
		double const RELAXATION_PARAMETER_AIRFLOW_MIN = 0.1;
		double const RELAXATION_PARAMETER_AIRFLOW_STEP = 0.1;
		double const CONVERGENCE_TOLERANCE_AIRFLOW = 1e-6;
	}

	namespace DeflectionConstants {
		double const YOUNGSMODULUS = 7.2e10;
		double const POISONRATIO = 0.22; // This constant is for glass
	}

}


#endif
