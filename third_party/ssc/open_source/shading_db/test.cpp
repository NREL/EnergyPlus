#include <iostream>
#include <vector>
#include <memory>
#include "lib_pv_shade_loss_mpp.h"
#include "core.h"

int main()
{
		std::vector<double> shad_fracs;
		// 1-8 string - example 2 string
		shad_fracs.push_back(50);
		shad_fracs.push_back(100);
		double gpoa = 140.826;
		double dpoa = 79.842;
		double pv_cell_temp = 9.6083;
		double mods_per_str = 12;
		double str_vmp_stc = 440.4;
		double mppt_lo = 250;
		double mppt_hi = 480;

		std::unique_ptr<ShadeDB8_mpp>  p_shade_db;
		p_shade_db = std::unique_ptr<ShadeDB8_mpp>(new ShadeDB8_mpp());

		p_shade_db->init();

		double dc_factor = 1.0 - p_shade_db->get_shade_loss(gpoa, dpoa, shad_fracs, true, pv_cell_temp, mods_per_str, str_vmp_stc, mppt_lo, mppt_hi);

		std::cout << "shading factor = " << dc_factor << "\n";
		std::cout << "warning messages = " << p_shade_db->get_warning() << "\n";
		std::cout << "error messages = " << p_shade_db->get_error() << "\n";

		return 0;
}
