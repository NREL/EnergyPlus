// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

// Google Test Headers
#include <vector>
#include <fstream>
#include <gtest/gtest.h>
#include <EnergyPlus/PhaseChangeModeling/HysteresisModel.hh>

using namespace EnergyPlus;

// A new one of these is created for each test
class HysteresisTest : public testing::Test
{
public:
	HysteresisPhaseChange::HysteresisPhaseChange ModelA;
	virtual void SetUp()
	{
		this->ModelA.name = "PCM Name";
		this->ModelA.totalLatentHeat = 25000;  // J/kg ?
		this->ModelA.specificHeatLiquid = 25000;  // J/kgK
		this->ModelA.deltaTempMeltingHigh = 1.0;  // deltaC
		this->ModelA.peakTempMelting = 20;  // degC
		this->ModelA.deltaTempMeltingLow = 1.0;  // deltaC
		this->ModelA.specificHeatSolid = 20000;  // J/kgK
		this->ModelA.deltaTempFreezingHigh = 1.0;  // deltaC
		this->ModelA.peakTempFreezing = 23;  // degC
		this->ModelA.deltaTempFreezingLow = 1.0;  // deltaC
		this->ModelA.specHeatTransition = ( this->ModelA.specificHeatSolid + this->ModelA.specificHeatLiquid ) / 2.0;
		this->ModelA.CpOld = this->ModelA.specificHeatSolid;
	}

	virtual void TearDown()
	{
	}
};

struct GetSpecHeatArgs
{
	Real64 previousTemperature;
	Real64 updatedTemperature;
	Real64 temperatureReverse;
	int previousPhaseChangeState;
	int expectedUpdatedPhaseChangeState;
	Real64 expectedSpecificHeat;

	GetSpecHeatArgs(Real64 _previousTemperature, Real64 _updatedTemperature, Real64 _temperatureReverse,
					int _previousPhaseChangeState, int _expectedUpdatedPhaseChangeState, Real64 _expectedSpecificHeat) :
			previousTemperature(_previousTemperature),
			updatedTemperature(_updatedTemperature),
			temperatureReverse(_temperatureReverse),
			previousPhaseChangeState(_previousPhaseChangeState),
			expectedUpdatedPhaseChangeState(_expectedUpdatedPhaseChangeState),
			expectedSpecificHeat(_expectedSpecificHeat)
	{}

};

void generate_test_data(HysteresisPhaseChange::HysteresisPhaseChange ModelA, std::string test_name, Real64 initial_temp, Real64 ending_temp, Real64 delta_temp, int initial_state, Real64 temp_reverse = -999)
{

	std::ofstream plot_file;
	plot_file.open ("hysteresis_plot.csv");

	int previous_pcm_state = initial_state;
	Real64 previous_temp = initial_temp;

	std::vector<std::string> code_lines;
	while( true )
	{
		Real64 updated_temp = previous_temp + delta_temp;
		int calculated_pcm_state = -999;
		Real64 calculated_cp = ModelA.getCurrentSpecificHeat(previous_temp, updated_temp, temp_reverse, previous_pcm_state, calculated_pcm_state);
		plot_file << updated_temp << "," << calculated_cp << "\n";
		std::stringstream prev_temp_ss;
		prev_temp_ss << std::fixed << std::setprecision(1) << previous_temp;
		std::string prev_temp_string = prev_temp_ss.str();
		std::stringstream updated_temp_ss;
		updated_temp_ss << std::fixed << std::setprecision(1) << updated_temp;
		std::string updated_temp_string = updated_temp_ss.str();
		code_lines.push_back("\targs_list.push_back(GetSpecHeatArgs(" + prev_temp_string + ", " + updated_temp_string + ", " + std::to_string(temp_reverse) + ", " + std::to_string(previous_pcm_state) + ", " + std::to_string(calculated_pcm_state) + ", " + std::to_string(calculated_cp) + "));\n");
		previous_pcm_state = calculated_pcm_state;
		previous_temp = updated_temp;
		if (delta_temp > 0 && previous_temp > ending_temp) break;
		if (delta_temp < 0 && previous_temp < ending_temp) break;
	}
	plot_file.close();

	std::ofstream plot_script_file;
	plot_script_file.open ("make_plot.plt");
	plot_script_file << "set terminal dumb\n";
	plot_script_file << "set datafile separator \",\"\n";
	plot_script_file << "set title \"Specific Heat = f(Temperature)\"\n";
	plot_script_file << "plot \"hysteresis_plot.csv\" every ::2 using 1:2 title \"Specific Heat = f(T)\"\n";
	plot_script_file << "set title \"Specific Heat = f(Time)\"\n";
	plot_script_file << "plot \"hysteresis_plot.csv\" every ::2 using 0:2 title \"Specific Heat = f(t)\"\n";
	plot_script_file.close();
	system( "gnuplot make_plot.plt > plot_output.txt" );

	std::ofstream code_file;
	code_file.open ("hysteresis_code.cpp");
	code_file << "// Generated by this call:\n";
	code_file << "// generate_test_data(this->ModelA, \"" + test_name + "\", " + std::to_string(initial_temp) + ", " + std::to_string(ending_temp) + ", " + std::to_string(delta_temp) + ", " + std::to_string(initial_state) + ");\n";
	code_file << "TEST_F(HysteresisTest, " << test_name << ")\n";
	code_file << "{\n";
	code_file << "\t// TestDescription\n";

	std::string line;
	std::ifstream myfile ("plot_output.txt");
	if (myfile.is_open())
	{
		while ( getline (myfile, line) )
		{
			code_file << "\t// " << line << "\n";
		}
		myfile.close();
	}
	code_file << "\tstd::vector<GetSpecHeatArgs> args_list;\n";
	for ( auto & code_line : code_lines )
	{
		code_file << code_line;
	}
	code_file << "\tfor ( auto & cp_call : args_list ) {\n";
	code_file << "\t\tint calculated_pcm_state = -99;\n";
	code_file << "\t\tReal64 calculated_cp = this->ModelA.getCurrentSpecificHeat(cp_call.previousTemperature, cp_call.updatedTemperature, cp_call.temperatureReverse, cp_call.previousPhaseChangeState, calculated_pcm_state);\n";
	code_file << "\t\tEXPECT_EQ(cp_call.expectedUpdatedPhaseChangeState, calculated_pcm_state);\n";
	code_file << "\t\tEXPECT_NEAR(cp_call.expectedSpecificHeat, calculated_cp, 1.0);\n";
	code_file << "\t}\n";
	code_file << "}\n";
	code_file.close();
}

void generate_test_data_two_directions(HysteresisPhaseChange::HysteresisPhaseChange ModelA, std::string test_name, Real64 initial_temp, Real64 dir_a_delta_temp, Real64 dir_a_ending_temp, Real64 dir_b_delta_temp, Real64 dir_b_ending_temp, int initial_state, Real64 temp_reverse = -999)
{

	std::ofstream plot_file;
	plot_file.open ("hysteresis_plot.csv");

	int previous_pcm_state = initial_state;
	Real64 previous_temp = initial_temp;

	int dir_counter = 1;

	std::vector<std::string> code_lines;
	while( true )
	{
		Real64 updated_temp;
		if (dir_counter == 1)
		{
			updated_temp = previous_temp + dir_a_delta_temp;
		} else {
			updated_temp = previous_temp + dir_b_delta_temp;
		}
		int calculated_pcm_state = -999;
		Real64 calculated_cp = ModelA.getCurrentSpecificHeat(previous_temp, updated_temp, temp_reverse, previous_pcm_state, calculated_pcm_state);
		plot_file << updated_temp << "," << calculated_cp << "\n";
		std::stringstream prev_temp_ss;
		prev_temp_ss << std::fixed << std::setprecision(1) << previous_temp;
		std::string prev_temp_string = prev_temp_ss.str();
		std::stringstream updated_temp_ss;
		updated_temp_ss << std::fixed << std::setprecision(1) << updated_temp;
		std::string updated_temp_string = updated_temp_ss.str();
		code_lines.push_back("\targs_list.push_back(GetSpecHeatArgs(" + prev_temp_string + ", " + updated_temp_string + ", " + std::to_string(temp_reverse) + ", " + std::to_string(previous_pcm_state) + ", " + std::to_string(calculated_pcm_state) + ", " + std::to_string(calculated_cp) + "));\n");
		previous_pcm_state = calculated_pcm_state;
		previous_temp = updated_temp;
		if (dir_counter == 1) {
			if (dir_a_delta_temp > 0 && previous_temp > dir_a_ending_temp) dir_counter++;
			if (dir_a_delta_temp < 0 && previous_temp < dir_a_ending_temp) dir_counter++;
		} else {
			if (dir_b_delta_temp > 0 && previous_temp > dir_b_ending_temp) break;
			if (dir_b_delta_temp < 0 && previous_temp < dir_b_ending_temp) break;
		}

	}
	plot_file.close();

	std::ofstream plot_script_file;
	plot_script_file.open ("make_plot.plt");
	plot_script_file << "set terminal dumb\n";
	plot_script_file << "set datafile separator \",\"\n";
	plot_script_file << "set title \"Specific Heat = f(Temperature)\"\n";
	plot_script_file << "plot \"hysteresis_plot.csv\" every ::2 using 1:2 title \"Specific Heat = f(T)\"\n";
	plot_script_file << "set title \"Specific Heat = f(Time)\"\n";
	plot_script_file << "plot \"hysteresis_plot.csv\" every ::2 using 0:2 title \"Specific Heat = f(t)\"\n";

	plot_script_file.close();
	system( "gnuplot make_plot.plt > plot_output.txt" );

	std::ofstream code_file;
	code_file.open ("hysteresis_code.cpp");
	code_file << "// Generated by this call:\n";
	code_file << "// generate_test_data_two_directions(this->ModelA, \"" + test_name + "\", " + std::to_string(initial_temp) + ", " + std::to_string(dir_a_delta_temp) + ", " + std::to_string(dir_a_ending_temp) + ", " + std::to_string(dir_b_delta_temp) + ", " + std::to_string(dir_b_ending_temp) + ", " + std::to_string(initial_state) + ");\n";
	code_file << "TEST_F(HysteresisTest, " << test_name << ")\n";
	code_file << "{\n";
	code_file << "\t// TestDescription\n";

	std::string line;
	std::ifstream myfile ("plot_output.txt");
	if (myfile.is_open())
	{
		while ( getline (myfile, line) )
		{
			code_file << "\t// " << line << "\n";
		}
		myfile.close();
	}
	code_file << "\tstd::vector<GetSpecHeatArgs> args_list;\n";
	for ( auto & code_line : code_lines )
	{
		code_file << code_line;
	}
	code_file << "\tfor ( auto & cp_call : args_list ) {\n";
	code_file << "\t\tint calculated_pcm_state = -99;\n";
	code_file << "\t\tReal64 calculated_cp = this->ModelA.getCurrentSpecificHeat(cp_call.previousTemperature, cp_call.updatedTemperature, cp_call.temperatureReverse, cp_call.previousPhaseChangeState, calculated_pcm_state);\n";
	code_file << "\t\tEXPECT_EQ(cp_call.expectedUpdatedPhaseChangeState, calculated_pcm_state);\n";
	code_file << "\t\tEXPECT_NEAR(cp_call.expectedSpecificHeat, calculated_cp, 1.0);\n";
	code_file << "\t}\n";
	code_file << "}\n";
	code_file.close();
}

TEST_F(HysteresisTest, GenerateTestData)
{
	//generate_test_data(this->ModelA, "StraightUpCurve", 14, 26, 0.5, 2);
	//generate_test_data(this->ModelA, "StraightDownCurve", 30, 15, -0.5, -2);
	//generate_test_data_two_directions(this->ModelA, "IntoMeltingAndBackDown", 14, 0.5, 20, -0.5, 14, 2);
	generate_test_data_two_directions(this->ModelA, "CompletelyThroughMeltingAndBackDown", 14, 0.5, 30, -0.5, 14, 2);
}

// Generated by this call:
// generate_test_data(this->ModelA, "StraightUpCurve", 14.000000, 26.000000, 0.500000, 2);
TEST_F(HysteresisTest, StraightUpCurve)
{
	// TestDescription
	//
	//                           Specific Heat = f(Temperature)
	//
	//   36000 +-+-------+--------+---------A--------+---------+--------+-------+-+
	//         +         +        +         +        +         +        +         +
	//   34000 +-+                            A      Specific Heat = f(T)    A  +-+
	//         |                                                                  |
	//         |                                                                  |
	//   32000 +-+                                                              +-+
	//         |                                                                  |
	//   30000 +-+                                                              +-+
	//         |                         A                                        |
	//   28000 +-+                               A                              +-+
	//         |                                                                  |
	//   26000 +-+                                 A                            +-+
	//         |                                     A  A A A  A A  A A A  A      |
	//   24000 +-+                                                              +-+
	//         |                       A                                          |
	//         |                                                                  |
	//   22000 +-+                                                              +-+
	//         +         +        A  A      +        +         +        +         +
	//   20000 +-+----A--A-A-A--A-+---------+--------+---------+--------+-------+-+
	//         14        16       18        20       22        24       26        28
	//
	//
	//                               Specific Heat = f(Time)
	//
	//   36000 +-+----------+----------A--+------------+-------------+----------+-+
	//         +            +             +            +             +            +
	//   34000 +-+                        A          Specific Heat = f(t)    A  +-+
	//         |                                                                  |
	//         |                                                                  |
	//   32000 +-+                                                              +-+
	//         |                                                                  |
	//   30000 +-+                                                              +-+
	//         |                    A                                             |
	//   28000 +-+                          A                                   +-+
	//         |                                                                  |
	//   26000 +-+                             A                                +-+
	//         |                                  A  A A  A  A A  A  A A  A       |
	//   24000 +-+                                                              +-+
	//         |                  A                                               |
	//         |                                                                  |
	//   22000 +-+                                                              +-+
	//         +            A  A          +            +             +            +
	//   20000 A-+A-A--A--A-+-------------+------------+-------------+----------+-+
	//         0            5             10           15            20           25
	//
	std::vector<GetSpecHeatArgs> args_list;
	args_list.push_back(GetSpecHeatArgs(14.0, 14.5, -999.000000, 2, 2, 20000.417543));
	args_list.push_back(GetSpecHeatArgs(14.5, 15.0, -999.000000, 2, 2, 20001.134998));
	args_list.push_back(GetSpecHeatArgs(15.0, 15.5, -999.000000, 2, 2, 20003.085245));
	args_list.push_back(GetSpecHeatArgs(15.5, 16.0, -999.000000, 2, 2, 20008.386566));
	args_list.push_back(GetSpecHeatArgs(16.0, 16.5, -999.000000, 2, 2, 20022.797049));
	args_list.push_back(GetSpecHeatArgs(16.5, 17.0, -999.000000, 2, 2, 20061.968804));
	args_list.push_back(GetSpecHeatArgs(17.0, 17.5, -999.000000, 2, 2, 20168.448675));
	args_list.push_back(GetSpecHeatArgs(17.5, 18.0, -999.000000, 2, 2, 20457.890972));
	args_list.push_back(GetSpecHeatArgs(18.0, 18.5, -999.000000, 2, 2, 21244.676709));
	args_list.push_back(GetSpecHeatArgs(18.5, 19.0, -999.000000, 2, -1, 23383.382081));
	args_list.push_back(GetSpecHeatArgs(19.0, 19.5, -999.000000, -1, -1, 29196.986029));
	args_list.push_back(GetSpecHeatArgs(19.5, 20.0, -999.000000, -1, -1, 35803.013971));
	args_list.push_back(GetSpecHeatArgs(20.0, 20.5, -999.000000, -1, -1, 34196.986029));
	args_list.push_back(GetSpecHeatArgs(20.5, 21.0, -999.000000, -1, -1, 28383.382081));
	args_list.push_back(GetSpecHeatArgs(21.0, 21.5, -999.000000, -1, -2, 26244.676709));
	args_list.push_back(GetSpecHeatArgs(21.5, 22.0, -999.000000, -2, -2, 25457.890972));
	args_list.push_back(GetSpecHeatArgs(22.0, 22.5, -999.000000, -2, -2, 25168.448675));
	args_list.push_back(GetSpecHeatArgs(22.5, 23.0, -999.000000, -2, -2, 25061.968804));
	args_list.push_back(GetSpecHeatArgs(23.0, 23.5, -999.000000, -2, -2, 25022.797049));
	args_list.push_back(GetSpecHeatArgs(23.5, 24.0, -999.000000, -2, -2, 25008.386566));
	args_list.push_back(GetSpecHeatArgs(24.0, 24.5, -999.000000, -2, -2, 25003.085245));
	args_list.push_back(GetSpecHeatArgs(24.5, 25.0, -999.000000, -2, -2, 25001.134998));
	args_list.push_back(GetSpecHeatArgs(25.0, 25.5, -999.000000, -2, -2, 25000.417543));
	args_list.push_back(GetSpecHeatArgs(25.5, 26.0, -999.000000, -2, -2, 25000.153605));
	args_list.push_back(GetSpecHeatArgs(26.0, 26.5, -999.000000, -2, -2, 25000.056508));
	for ( auto & cp_call : args_list ) {
		int calculated_pcm_state = -99;
		Real64 calculated_cp = this->ModelA.getCurrentSpecificHeat(cp_call.previousTemperature, cp_call.updatedTemperature, cp_call.temperatureReverse, cp_call.previousPhaseChangeState, calculated_pcm_state);
		EXPECT_EQ(cp_call.expectedUpdatedPhaseChangeState, calculated_pcm_state);
		EXPECT_NEAR(cp_call.expectedSpecificHeat, calculated_cp, 1.0);
	}
}


// Generated by this call:
// generate_test_data(this->ModelA, "StraightDownCurve", 30.000000, 15.000000, -0.500000, -2);
TEST_F(HysteresisTest, StraightDownCurve)
{
	// TestDescription
	//
	//                           Specific Heat = f(Temperature)
	//
	//   45000 +-+-----+--------+-------+--------+-------+-------+--------+-----+-+
	//         +       +        +       +        +       +       +        +       +
	//         |                                     Specific Heat = f(T)    A    |
	//         |                                     A                            |
	//   40000 +-+                                                              +-+
	//         |                                                                  |
	//         |                                                                  |
	//   35000 +-+                                                              +-+
	//         |                                       A                          |
	//         |                                                                  |
	//         |                                                                  |
	//   30000 +-+                                 A                            +-+
	//         |                                         A                        |
	//         |                                                                  |
	//   25000 +-+                                         A A A A A A  A A A   +-+
	//         |                                                                  |
	//         |                                 A                                |
	//         +       +        +       +     A  +       +       +        +       +
	//   20000 +-A-A-A-A-A--A-A-A-A-A-A-A-A-A----+-------+-------+--------+-----+-+
	//         14      16       18      20       22      24      26       28      30
	//
	//
	//                               Specific Heat = f(Time)
	//
	//   45000 +-+--------+----------+-----------+----------+----------+--------+-+
	//         +          +          +           +          +          +          +
	//         |                                     Specific Heat = f(t)    A    |
	//         |                        A                                         |
	//   40000 +-+                                                              +-+
	//         |                                                                  |
	//         |                                                                  |
	//   35000 +-+                                                              +-+
	//         |                     A                                            |
	//         |                                                                  |
	//         |                                                                  |
	//   30000 +-+                        A                                     +-+
	//         |                   A                                              |
	//         |                                                                  |
	//   25000 A-A A  A A A A  A A                                              +-+
	//         |                                                                  |
	//         |                            A                                     |
	//         +          +          +        A  +          +          +          +
	//   20000 +-+--------+----------+-----------A-A-A-A-A--A-A-A-A--A-A-A-A--A-+-+
	//         0          5          10          15         20         25         30
	//
	std::vector<GetSpecHeatArgs> args_list;
	args_list.push_back(GetSpecHeatArgs(30.0, 29.5, -999.000000, -2, -2, 25000.056508));
	args_list.push_back(GetSpecHeatArgs(29.5, 29.0, -999.000000, -2, -2, 25000.153605));
	args_list.push_back(GetSpecHeatArgs(29.0, 28.5, -999.000000, -2, -2, 25000.417543));
	args_list.push_back(GetSpecHeatArgs(28.5, 28.0, -999.000000, -2, -2, 25001.134998));
	args_list.push_back(GetSpecHeatArgs(28.0, 27.5, -999.000000, -2, -2, 25003.085245));
	args_list.push_back(GetSpecHeatArgs(27.5, 27.0, -999.000000, -2, -2, 25008.386566));
	args_list.push_back(GetSpecHeatArgs(27.0, 26.5, -999.000000, -2, -2, 25022.797049));
	args_list.push_back(GetSpecHeatArgs(26.5, 26.0, -999.000000, -2, -2, 25061.968804));
	args_list.push_back(GetSpecHeatArgs(26.0, 25.5, -999.000000, -2, -2, 25168.448675));
	args_list.push_back(GetSpecHeatArgs(25.5, 25.0, -999.000000, -2, -2, 25457.890972));
	args_list.push_back(GetSpecHeatArgs(25.0, 24.5, -999.000000, -2, -2, 26244.676709));
	args_list.push_back(GetSpecHeatArgs(24.5, 24.0, -999.000000, -2, 1, 28383.382081));
	args_list.push_back(GetSpecHeatArgs(24.0, 23.5, -999.000000, 1, 1, 34196.986029));
	args_list.push_back(GetSpecHeatArgs(23.5, 23.0, -999.000000, 1, 1, 40803.013971));
	args_list.push_back(GetSpecHeatArgs(23.0, 22.5, -999.000000, 1, 1, 29196.986029));
	args_list.push_back(GetSpecHeatArgs(22.5, 22.0, -999.000000, 1, 1, 23383.382081));
	args_list.push_back(GetSpecHeatArgs(22.0, 21.5, -999.000000, 1, 2, 21244.676709));
	args_list.push_back(GetSpecHeatArgs(21.5, 21.0, -999.000000, 2, 2, 20457.890972));
	args_list.push_back(GetSpecHeatArgs(21.0, 20.5, -999.000000, 2, 2, 20168.448675));
	args_list.push_back(GetSpecHeatArgs(20.5, 20.0, -999.000000, 2, 2, 20061.968804));
	args_list.push_back(GetSpecHeatArgs(20.0, 19.5, -999.000000, 2, 2, 20022.797049));
	args_list.push_back(GetSpecHeatArgs(19.5, 19.0, -999.000000, 2, 2, 20008.386566));
	args_list.push_back(GetSpecHeatArgs(19.0, 18.5, -999.000000, 2, 2, 20003.085245));
	args_list.push_back(GetSpecHeatArgs(18.5, 18.0, -999.000000, 2, 2, 20001.134998));
	args_list.push_back(GetSpecHeatArgs(18.0, 17.5, -999.000000, 2, 2, 20000.417543));
	args_list.push_back(GetSpecHeatArgs(17.5, 17.0, -999.000000, 2, 2, 20000.153605));
	args_list.push_back(GetSpecHeatArgs(17.0, 16.5, -999.000000, 2, 2, 20000.056508));
	args_list.push_back(GetSpecHeatArgs(16.5, 16.0, -999.000000, 2, 2, 20000.020788));
	args_list.push_back(GetSpecHeatArgs(16.0, 15.5, -999.000000, 2, 2, 20000.007648));
	args_list.push_back(GetSpecHeatArgs(15.5, 15.0, -999.000000, 2, 2, 20000.002813));
	args_list.push_back(GetSpecHeatArgs(15.0, 14.5, -999.000000, 2, 2, 20000.001035));
	for ( auto & cp_call : args_list ) {
		int calculated_pcm_state = -99;
		Real64 calculated_cp = this->ModelA.getCurrentSpecificHeat(cp_call.previousTemperature, cp_call.updatedTemperature, cp_call.temperatureReverse, cp_call.previousPhaseChangeState, calculated_pcm_state);
		EXPECT_EQ(cp_call.expectedUpdatedPhaseChangeState, calculated_pcm_state);
		EXPECT_NEAR(cp_call.expectedSpecificHeat, calculated_cp, 1.0);
	}
}

// Generated by this call:
// generate_test_data_two_directions(this->ModelA, "CompletelyThroughMeltingAndBackDown", 14.000000, 0.500000, 30.000000, -0.500000, 14.000000, 2);
TEST_F(HysteresisTest, CompletelyThroughMeltingAndBackDown)
{
	// This test demonstrates that the specific heat can ride one curve up and one back down
	// It is a bit difficult to see on this plot exactly what is going on, but basically,
	// The specific heat starts at the fully solid specific heat (20000), and rides the first,
	// lower, bump while going up temperature, all the way through the curve, landing at the
	// liquid specific heat value of 25000.  Then, the curve goes back down, this time
	// riding the second, higher, bump and landing at the solid specific heat again.
	// The hysteresis is present in this test, but no transition calculations are utilized.
	//
	//                           Specific Heat = f(Temperature)
	//
	//   45000 +-+----+-----+------+------+------+-----+------+------+-----+----+-+
	//         +      +     +      +      +      +     +      +      +     +      +
	//         |                                     Specific Heat = f(T)    A    |
	//         |                                    A                             |
	//   40000 +-+                                                              +-+
	//         |                                                                  |
	//         |                                                                  |
	//   35000 +-+                        A                                     +-+
	//         |                           A          A                           |
	//         |                                                                  |
	//         |                                                                  |
	//   30000 +-+                      A         A                             +-+
	//         |                             A         A                          |
	//         |                                                                  |
	//   25000 +-+                             A AA A AA A AA A AA A AA A AA A  +-+
	//         |                                                                  |
	//         |                      A          A                                |
	//         +      +     +      + A    +    A +     +      +      +     +      +
	//   20000 +-+--A-AA-A-AA-A-AA-A-AA-A-AA-A---+-----+------+------+-----+----+-+
	//         12     14    16     18     20     22    24     26     28    30     32
	//
	//
	//                               Specific Heat = f(Time)
	//
	//   45000 +-+-------+--------+---------+--------+---------+--------+-------+-+
	//         +         +        +         +        +         +        +         +
	//         |                                     Specific Heat = f(t)    A    |
	//         |                                          A                       |
	//   40000 +-+                                                              +-+
	//         |                                                                  |
	//         |                                                                  |
	//   35000 +-+      A                                                       +-+
	//         |         A                               A                        |
	//         |                                                                  |
	//         |                                                                  |
	//   30000 +-+     A                                   A                    +-+
	//         |          A                             A                         |
	//         |                                                                  |
	//   25000 +-+        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA                        +-+
	//         |                                                                  |
	//         |      A                                     A                     |
	//         +     A   +        +         +        +       A +        +         +
	//   20000 AAAAAA----+--------+---------+--------+--------AAAAAAAAAAAAAAA---+-+
	//         0         10       20        30       40        50       60        70
	//
	std::vector<GetSpecHeatArgs> args_list;
	args_list.push_back(GetSpecHeatArgs(14.0, 14.5, -999.000000, 2, 2, 20000.417543));
	args_list.push_back(GetSpecHeatArgs(14.5, 15.0, -999.000000, 2, 2, 20001.134998));
	args_list.push_back(GetSpecHeatArgs(15.0, 15.5, -999.000000, 2, 2, 20003.085245));
	args_list.push_back(GetSpecHeatArgs(15.5, 16.0, -999.000000, 2, 2, 20008.386566));
	args_list.push_back(GetSpecHeatArgs(16.0, 16.5, -999.000000, 2, 2, 20022.797049));
	args_list.push_back(GetSpecHeatArgs(16.5, 17.0, -999.000000, 2, 2, 20061.968804));
	args_list.push_back(GetSpecHeatArgs(17.0, 17.5, -999.000000, 2, 2, 20168.448675));
	args_list.push_back(GetSpecHeatArgs(17.5, 18.0, -999.000000, 2, 2, 20457.890972));
	args_list.push_back(GetSpecHeatArgs(18.0, 18.5, -999.000000, 2, 2, 21244.676709));
	args_list.push_back(GetSpecHeatArgs(18.5, 19.0, -999.000000, 2, -1, 23383.382081));
	args_list.push_back(GetSpecHeatArgs(19.0, 19.5, -999.000000, -1, -1, 29196.986029));
	args_list.push_back(GetSpecHeatArgs(19.5, 20.0, -999.000000, -1, -1, 35803.013971));
	args_list.push_back(GetSpecHeatArgs(20.0, 20.5, -999.000000, -1, -1, 34196.986029));
	args_list.push_back(GetSpecHeatArgs(20.5, 21.0, -999.000000, -1, -1, 28383.382081));
	args_list.push_back(GetSpecHeatArgs(21.0, 21.5, -999.000000, -1, -2, 26244.676709));
	args_list.push_back(GetSpecHeatArgs(21.5, 22.0, -999.000000, -2, -2, 25457.890972));
	args_list.push_back(GetSpecHeatArgs(22.0, 22.5, -999.000000, -2, -2, 25168.448675));
	args_list.push_back(GetSpecHeatArgs(22.5, 23.0, -999.000000, -2, -2, 25061.968804));
	args_list.push_back(GetSpecHeatArgs(23.0, 23.5, -999.000000, -2, -2, 25022.797049));
	args_list.push_back(GetSpecHeatArgs(23.5, 24.0, -999.000000, -2, -2, 25008.386566));
	args_list.push_back(GetSpecHeatArgs(24.0, 24.5, -999.000000, -2, -2, 25003.085245));
	args_list.push_back(GetSpecHeatArgs(24.5, 25.0, -999.000000, -2, -2, 25001.134998));
	args_list.push_back(GetSpecHeatArgs(25.0, 25.5, -999.000000, -2, -2, 25000.417543));
	args_list.push_back(GetSpecHeatArgs(25.5, 26.0, -999.000000, -2, -2, 25000.153605));
	args_list.push_back(GetSpecHeatArgs(26.0, 26.5, -999.000000, -2, -2, 25000.056508));
	args_list.push_back(GetSpecHeatArgs(26.5, 27.0, -999.000000, -2, -2, 25000.020788));
	args_list.push_back(GetSpecHeatArgs(27.0, 27.5, -999.000000, -2, -2, 25000.007648));
	args_list.push_back(GetSpecHeatArgs(27.5, 28.0, -999.000000, -2, -2, 25000.002813));
	args_list.push_back(GetSpecHeatArgs(28.0, 28.5, -999.000000, -2, -2, 25000.001035));
	args_list.push_back(GetSpecHeatArgs(28.5, 29.0, -999.000000, -2, -2, 25000.000381));
	args_list.push_back(GetSpecHeatArgs(29.0, 29.5, -999.000000, -2, -2, 25000.000140));
	args_list.push_back(GetSpecHeatArgs(29.5, 30.0, -999.000000, -2, -2, 25000.000052));
	args_list.push_back(GetSpecHeatArgs(30.0, 30.5, -999.000000, -2, -2, 25000.000019));
	args_list.push_back(GetSpecHeatArgs(30.5, 30.0, -999.000000, -2, -2, 25000.020788));
	args_list.push_back(GetSpecHeatArgs(30.0, 29.5, -999.000000, -2, -2, 25000.056508));
	args_list.push_back(GetSpecHeatArgs(29.5, 29.0, -999.000000, -2, -2, 25000.153605));
	args_list.push_back(GetSpecHeatArgs(29.0, 28.5, -999.000000, -2, -2, 25000.417543));
	args_list.push_back(GetSpecHeatArgs(28.5, 28.0, -999.000000, -2, -2, 25001.134998));
	args_list.push_back(GetSpecHeatArgs(28.0, 27.5, -999.000000, -2, -2, 25003.085245));
	args_list.push_back(GetSpecHeatArgs(27.5, 27.0, -999.000000, -2, -2, 25008.386566));
	args_list.push_back(GetSpecHeatArgs(27.0, 26.5, -999.000000, -2, -2, 25022.797049));
	args_list.push_back(GetSpecHeatArgs(26.5, 26.0, -999.000000, -2, -2, 25061.968804));
	args_list.push_back(GetSpecHeatArgs(26.0, 25.5, -999.000000, -2, -2, 25168.448675));
	args_list.push_back(GetSpecHeatArgs(25.5, 25.0, -999.000000, -2, -2, 25457.890972));
	args_list.push_back(GetSpecHeatArgs(25.0, 24.5, -999.000000, -2, -2, 26244.676709));
	args_list.push_back(GetSpecHeatArgs(24.5, 24.0, -999.000000, -2, 1, 28383.382081));
	args_list.push_back(GetSpecHeatArgs(24.0, 23.5, -999.000000, 1, 1, 34196.986029));
	args_list.push_back(GetSpecHeatArgs(23.5, 23.0, -999.000000, 1, 1, 40803.013971));
	args_list.push_back(GetSpecHeatArgs(23.0, 22.5, -999.000000, 1, 1, 29196.986029));
	args_list.push_back(GetSpecHeatArgs(22.5, 22.0, -999.000000, 1, 1, 23383.382081));
	args_list.push_back(GetSpecHeatArgs(22.0, 21.5, -999.000000, 1, 2, 21244.676709));
	args_list.push_back(GetSpecHeatArgs(21.5, 21.0, -999.000000, 2, 2, 20457.890972));
	args_list.push_back(GetSpecHeatArgs(21.0, 20.5, -999.000000, 2, 2, 20168.448675));
	args_list.push_back(GetSpecHeatArgs(20.5, 20.0, -999.000000, 2, 2, 20061.968804));
	args_list.push_back(GetSpecHeatArgs(20.0, 19.5, -999.000000, 2, 2, 20022.797049));
	args_list.push_back(GetSpecHeatArgs(19.5, 19.0, -999.000000, 2, 2, 20008.386566));
	args_list.push_back(GetSpecHeatArgs(19.0, 18.5, -999.000000, 2, 2, 20003.085245));
	args_list.push_back(GetSpecHeatArgs(18.5, 18.0, -999.000000, 2, 2, 20001.134998));
	args_list.push_back(GetSpecHeatArgs(18.0, 17.5, -999.000000, 2, 2, 20000.417543));
	args_list.push_back(GetSpecHeatArgs(17.5, 17.0, -999.000000, 2, 2, 20000.153605));
	args_list.push_back(GetSpecHeatArgs(17.0, 16.5, -999.000000, 2, 2, 20000.056508));
	args_list.push_back(GetSpecHeatArgs(16.5, 16.0, -999.000000, 2, 2, 20000.020788));
	args_list.push_back(GetSpecHeatArgs(16.0, 15.5, -999.000000, 2, 2, 20000.007648));
	args_list.push_back(GetSpecHeatArgs(15.5, 15.0, -999.000000, 2, 2, 20000.002813));
	args_list.push_back(GetSpecHeatArgs(15.0, 14.5, -999.000000, 2, 2, 20000.001035));
	args_list.push_back(GetSpecHeatArgs(14.5, 14.0, -999.000000, 2, 2, 20000.000381));
	args_list.push_back(GetSpecHeatArgs(14.0, 13.5, -999.000000, 2, 2, 20000.000140));
	for ( auto & cp_call : args_list ) {
		int calculated_pcm_state = -99;
		Real64 calculated_cp = this->ModelA.getCurrentSpecificHeat(cp_call.previousTemperature, cp_call.updatedTemperature, cp_call.temperatureReverse, cp_call.previousPhaseChangeState, calculated_pcm_state);
		EXPECT_EQ(cp_call.expectedUpdatedPhaseChangeState, calculated_pcm_state);
		EXPECT_NEAR(cp_call.expectedSpecificHeat, calculated_cp, 1.0);
	}
}

// Generated by this call:
// generate_test_data_two_directions(this->ModelA, "IntoMeltingAndBackDown", 14.000000, 0.500000, 20.000000, -0.500000, 14.000000, 2);
TEST_F(HysteresisTest, IntoMeltingAndBackDown)
{
	// This test demonstrates a transition.  The PCM goes from the solid state, begins melting,
	// and then cools back off.
	//
	//                           Specific Heat = f(Temperature)
	//
	//   36000 +-+-----+--------+-------+--------+-------+-------+--------A-----+-+
	//         +       +        +       +        +       +       +        +       +
	//   34000 +-+                                   Specific Heat = f(T)    AA +-+
	//         |                                                                  |
	//         |                                                                  |
	//   32000 +-+                                                              +-+
	//         |                                                                  |
	//   30000 +-+                                                              +-+
	//         |                                                     A            |
	//   28000 +-+                                                              +-+
	//         |                                                                  |
	//   26000 +-+                                                              +-+
	//         |                                                                  |
	//   24000 +-+                                                              +-+
	//         |                                                 A                |
	//         |                                                                  |
	//   22000 +-+                                                              +-+
	//         +       +        +       +        +       A   A   +        +       +
	//   20000 +-+-A---A----A---A---A---A---A----A---A---A---A---A---A----A-----+-+
	//         13      14       15      16       17      18      19       20      21
	//
	//
	//                               Specific Heat = f(Time)
	//
	//   36000 +-+----------+----------A--+------------+-------------+----------+-+
	//         +            +             +            +             +            +
	//   34000 +-+                        A          Specific Heat = f(t)    A  +-+
	//         |                                                                  |
	//         |                                                                  |
	//   32000 +-+                                                              +-+
	//         |                                                                  |
	//   30000 +-+                                                              +-+
	//         |                    A                                             |
	//   28000 +-+                                                              +-+
	//         |                                                                  |
	//   26000 +-+                                                              +-+
	//         |                                                                  |
	//   24000 +-+                                                              +-+
	//         |                  A                                               |
	//         |                                                                  |
	//   22000 +-+                                                              +-+
	//         +            A  A          +            +             +            +
	//   20000 A-+A-A--A--A-+-------------+-A--A--A--A-A--A--A-A--A--A-A--A--A-A+-+
	//         0            5             10           15            20           25
	//
	std::vector<GetSpecHeatArgs> args_list;
	args_list.push_back(GetSpecHeatArgs(14.0, 14.5, -999.000000, 2, 2, 20000.417543));
	args_list.push_back(GetSpecHeatArgs(14.5, 15.0, -999.000000, 2, 2, 20001.134998));
	args_list.push_back(GetSpecHeatArgs(15.0, 15.5, -999.000000, 2, 2, 20003.085245));
	args_list.push_back(GetSpecHeatArgs(15.5, 16.0, -999.000000, 2, 2, 20008.386566));
	args_list.push_back(GetSpecHeatArgs(16.0, 16.5, -999.000000, 2, 2, 20022.797049));
	args_list.push_back(GetSpecHeatArgs(16.5, 17.0, -999.000000, 2, 2, 20061.968804));
	args_list.push_back(GetSpecHeatArgs(17.0, 17.5, -999.000000, 2, 2, 20168.448675));
	args_list.push_back(GetSpecHeatArgs(17.5, 18.0, -999.000000, 2, 2, 20457.890972));
	args_list.push_back(GetSpecHeatArgs(18.0, 18.5, -999.000000, 2, 2, 21244.676709));
	args_list.push_back(GetSpecHeatArgs(18.5, 19.0, -999.000000, 2, -1, 23383.382081));
	args_list.push_back(GetSpecHeatArgs(19.0, 19.5, -999.000000, -1, -1, 29196.986029));
	args_list.push_back(GetSpecHeatArgs(19.5, 20.0, -999.000000, -1, -1, 35803.013971));
	args_list.push_back(GetSpecHeatArgs(20.0, 20.5, -999.000000, -1, -1, 34196.986029));
	args_list.push_back(GetSpecHeatArgs(20.5, 20.0, -999.000000, -1, 2, 20061.968804));
	args_list.push_back(GetSpecHeatArgs(20.0, 19.5, -999.000000, 2, 2, 20022.797049));
	args_list.push_back(GetSpecHeatArgs(19.5, 19.0, -999.000000, 2, 2, 20008.386566));
	args_list.push_back(GetSpecHeatArgs(19.0, 18.5, -999.000000, 2, 2, 20003.085245));
	args_list.push_back(GetSpecHeatArgs(18.5, 18.0, -999.000000, 2, 2, 20001.134998));
	args_list.push_back(GetSpecHeatArgs(18.0, 17.5, -999.000000, 2, 2, 20000.417543));
	args_list.push_back(GetSpecHeatArgs(17.5, 17.0, -999.000000, 2, 2, 20000.153605));
	args_list.push_back(GetSpecHeatArgs(17.0, 16.5, -999.000000, 2, 2, 20000.056508));
	args_list.push_back(GetSpecHeatArgs(16.5, 16.0, -999.000000, 2, 2, 20000.020788));
	args_list.push_back(GetSpecHeatArgs(16.0, 15.5, -999.000000, 2, 2, 20000.007648));
	args_list.push_back(GetSpecHeatArgs(15.5, 15.0, -999.000000, 2, 2, 20000.002813));
	args_list.push_back(GetSpecHeatArgs(15.0, 14.5, -999.000000, 2, 2, 20000.001035));
	args_list.push_back(GetSpecHeatArgs(14.5, 14.0, -999.000000, 2, 2, 20000.000381));
	args_list.push_back(GetSpecHeatArgs(14.0, 13.5, -999.000000, 2, 2, 20000.000140));
	for ( auto & cp_call : args_list ) {
		int calculated_pcm_state = -99;
		Real64 calculated_cp = this->ModelA.getCurrentSpecificHeat(cp_call.previousTemperature, cp_call.updatedTemperature, cp_call.temperatureReverse, cp_call.previousPhaseChangeState, calculated_pcm_state);
		EXPECT_EQ(cp_call.expectedUpdatedPhaseChangeState, calculated_pcm_state);
		EXPECT_NEAR(cp_call.expectedSpecificHeat, calculated_cp, 1.0);
	}
}