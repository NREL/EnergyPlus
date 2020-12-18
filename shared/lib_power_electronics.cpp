#include "lib_battery_dispatch.h"
#include "lib_battery_powerflow.h"
#include "lib_power_electronics.h"

double BatteryBidirectionalInverter::convert_to_dc(double P_ac, double * P_dc)
{
	double P_loss = P_ac * (1 - _ac_dc_efficiency);
	*P_dc = P_ac * _ac_dc_efficiency;
	return P_loss;
}
double BatteryBidirectionalInverter::convert_to_ac(double P_dc, double * P_ac)
{
	double P_loss = P_dc * (1 - _dc_ac_efficiency);
	*P_ac = P_dc * _dc_ac_efficiency;
	return P_loss;
}
double BatteryBidirectionalInverter::compute_dc_from_ac(double P_ac)
{
	return P_ac / _dc_ac_efficiency;
}

double BatteryRectifier::convert_to_dc(double P_ac, double * P_dc)
{
	double P_loss = P_ac * (1 - _ac_dc_efficiency);
	*P_dc = P_ac * _ac_dc_efficiency;
	return P_loss;
}

ChargeController::ChargeController(dispatch_t * dispatch, battery_metrics_t * battery_metrics) :
	m_batteryMetrics(battery_metrics),
	m_dispatch(dispatch)
{
}

ACBatteryController::ACBatteryController(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double efficiencyACToDC, double efficiencyDCToAC) : ChargeController(dispatch, battery_metrics)
{
	std::unique_ptr<BatteryBidirectionalInverter> tmp(new BatteryBidirectionalInverter(efficiencyACToDC, efficiencyDCToAC));
	m_bidirectionalInverter = std::move(tmp);
	m_batteryPower = dispatch->getBatteryPower();
	m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;
	m_batteryPower->singlePointEfficiencyACToDC = m_bidirectionalInverter->ac_dc_efficiency();
	m_batteryPower->singlePointEfficiencyDCToAC = m_bidirectionalInverter->dc_ac_efficiency();
}

void ACBatteryController::run(size_t year, size_t hour_of_year, size_t step_of_hour, size_t)
{
	if (m_batteryPower->powerSystem < 0)
	{
		m_batteryPower->powerPVInverterDraw = m_batteryPower->powerSystem;
		m_batteryPower->powerSystem = 0;
	}

	// For AC connected system, there is no power going through shared inverter
	m_batteryPower->powerSystemThroughSharedInverter = 0;
	m_batteryPower->powerSystemClipped = 0;

	// Dispatch the battery
	m_dispatch->dispatch(year, hour_of_year, step_of_hour);

	// Compute annual metrics
	m_batteryMetrics->compute_metrics_ac(m_dispatch->getBatteryPower());
}

DCBatteryController::DCBatteryController(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double efficiencyDCToDC, double inverterEfficiencyCutoff) 
	: ChargeController(dispatch, battery_metrics)
{
	std::unique_ptr<Battery_DC_DC_ChargeController> tmp(new Battery_DC_DC_ChargeController(efficiencyDCToDC, 100));
	m_DCDCChargeController = std::move(tmp);
	m_batteryPower = dispatch->getBatteryPower();
	m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;
	m_batteryPower->singlePointEfficiencyDCToDC = m_DCDCChargeController->batt_dc_dc_bms_efficiency();
	m_batteryPower->inverterEfficiencyCutoff = inverterEfficiencyCutoff;
}

void DCBatteryController::setSharedInverter(SharedInverter * sharedInverter)
{
	m_batteryPower->setSharedInverter(sharedInverter);
}

void DCBatteryController::run(size_t year, size_t hour_of_year, size_t step_of_hour, size_t)
{
	if (m_batteryPower->powerSystem < 0){
		m_batteryPower->powerSystem = 0;
	}

	// For DC connected system, there is potentially full PV power going through shared inverter
	m_batteryPower->powerSystemThroughSharedInverter = m_batteryPower->powerSystem;

	// Dispatch the battery
	m_dispatch->dispatch(year, hour_of_year, step_of_hour);

	// Compute annual metrics
	m_batteryMetrics->compute_metrics_ac(m_dispatch->getBatteryPower());
}
