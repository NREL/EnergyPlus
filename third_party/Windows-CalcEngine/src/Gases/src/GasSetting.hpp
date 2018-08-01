#ifndef GASSETTING_H
#define GASSETTING_H

namespace Gases {

	class CGasSettings {
	public:
		static CGasSettings& instance();

		double getVacuumPressure() const;
		void setVacuumPressure( double const t_Value );

	private:
		CGasSettings();

		// Value that will trigger specific gas calculations. Bellow this value it will be considered
		// that gases need to apply vacuum calculations.
		double m_VacuumPressure;
	};

}

#endif
