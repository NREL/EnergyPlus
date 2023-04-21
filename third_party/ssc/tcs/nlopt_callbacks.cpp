//#include "nlopt_callbacks.h"
//#include "sco2_power_cycle.h"
//
//double nlopt_callback_opt_des(const std::vector<double> &x, std::vector<double> &grad, void *data)
//{
//	RecompCycle *frame = static_cast<RecompCycle*>(data);
//	if( frame != NULL ) return frame->design_point_eta(x);
//}
//
//double nlopt_callback_opt_off_des(const std::vector<double> &x, std::vector<double> &grad, void *data)
//{
//	RecompCycle *frame = static_cast<RecompCycle*>(data);
//	if( frame != NULL) return frame->off_design_target_power_function(x);
//}

