#include "core.h"

#include "ud_power_cycle.h"

static var_info _cm_vtab_test_ud_power_cycle[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                          UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{SSC_INPUT, SSC_NUMBER, "q_pb_design", "Design point power block thermal power", "MWt", "", "", "", "", ""},

	{SSC_OUTPUT, SSC_NUMBER, "W_dot_fossil", "Electric output with no solar contribution", "MWe", "", "", "", "", ""},

	var_info_invalid};

class cm_test_ud_power_cycle : public compute_module
{
public:

	cm_test_ud_power_cycle()
	{
		add_var_info(_cm_vtab_test_ud_power_cycle);
	}

	void exec() override
	{
		double a_ref = 12.0;
		double b_ref = 13.0;
		double c_ref = 14.0;
		double Y_ref = three_var_eqn(a_ref, b_ref, c_ref);

		double a_low = 10.0;
		double a_high = 14.0;

		double b_low = 10.0;
		double b_high = 16.0;

		double c_low = 10.0;
		double c_high = 18.0;

		int N_runs = 20;

		util::matrix_t<double> a_table(N_runs, 13, 1.0);
		util::matrix_t<double> b_table(N_runs, 13, 1.0);
		util::matrix_t<double> c_table(N_runs, 13, 1.0);

		for(int i = 0; i < N_runs; i++)
		{
			a_table(i,0) = a_low + (a_high-a_low)/(double)(N_runs-1)*i;
			a_table(i,1) = three_var_eqn(a_table(i,0),b_ref,c_low)/Y_ref;
			a_table(i,2) = three_var_eqn(a_table(i,0),b_ref,c_ref)/Y_ref;
			a_table(i,3) = three_var_eqn(a_table(i,0),b_ref,c_high)/Y_ref;

			b_table(i,0) = b_low + (b_high-b_low)/(double)(N_runs-1)*i;
			b_table(i,1) = three_var_eqn(a_low,b_table(i,0),c_ref)/Y_ref;
			b_table(i,2) = three_var_eqn(a_ref,b_table(i,0),c_ref)/Y_ref;
			b_table(i,3) = three_var_eqn(a_high,b_table(i,0),c_ref)/Y_ref;

			c_table(i,0) = c_low + (c_high-c_low)/(double)(N_runs-1)*i;
			c_table(i,1) = three_var_eqn(a_ref,b_low,c_table(i,0))/Y_ref;
			c_table(i,2) = three_var_eqn(a_ref,b_ref,c_table(i,0))/Y_ref;
			c_table(i,3) = three_var_eqn(a_ref,b_high,c_table(i,0))/Y_ref;
		}

		C_ud_power_cycle c_pc;

		c_pc.init(a_table, a_ref, a_low, a_high,
				b_table, b_ref, b_low, b_high,
				c_table, c_ref, c_low, c_high);

		int n_test = N_runs*N_runs*N_runs;
		
		std::vector<double> Y_actual(n_test);
		std::vector<double> Y_reg(n_test);
		std::vector<double> E_reg_less_act(n_test);

		double max_err = -1.0;

		for(int i = 0; i < N_runs; i++)
		{
			for(int j = 0; j < N_runs; j++)
			{
				for(int k = 0; k < N_runs; k++)
				{
					int index = i*N_runs*N_runs + j*N_runs + k;

					Y_actual[index] = three_var_eqn(a_table(i,0),b_table(j,0),c_table(k,0));

					Y_reg[index] = c_pc.get_W_dot_gross_ND(a_table(i,0),b_table(j,0),c_table(k,0))*Y_ref;

					E_reg_less_act[index] = (Y_reg[index] - Y_actual[index])/fmax(Y_actual[index],0.0001);

					if(fabs(E_reg_less_act[index]) > max_err)
					{
						max_err = fabs(E_reg_less_act[index]);
					}
				}
			}
		}
	}

	double three_var_eqn(double a, double b, double c)
	{
		return a*pow(b,1.24) - a/pow(c,0.55) + b/(2*c+1.0)*a;
		//return 1.0;
	}

};

DEFINE_MODULE_ENTRY(test_ud_power_cycle, "Test user-defined power cylce model", 0)
