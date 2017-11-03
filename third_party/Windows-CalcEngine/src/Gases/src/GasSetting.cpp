#include "WCECommon.hpp"
#include "GasSetting.hpp"

namespace Gases {

	CGasSettings& CGasSettings::instance() {
		static CGasSettings p_inst;
		return p_inst;
	}

	double CGasSettings::getVacuumPressure() const {
		return m_VacuumPressure;
	}

	void CGasSettings::setVacuumPressure( double const t_Value ) {
		m_VacuumPressure = t_Value;
	}

	CGasSettings::CGasSettings() : m_VacuumPressure( ConstantsData::VACUUMPRESSURE ) {

	}

}
