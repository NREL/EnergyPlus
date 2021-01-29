/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

// To duplicate functionality of GetShadeLoss.m from Sara for shade loss database
/* from GetShadeLoss.m:
GetShadeLoss(G,D,Tc,ModsPerString,StrShade,VMaxSTCStrUnshaded,VStrMPPT,ShadeDB )
% This searches the shade database and returns the %loss from partial shading
% G is the global POA irradiance, D is the diffuse irradiance, Tc is PV cell
% temperature, StrShade is a vector with each string's shaded fraction (like 24, 55, 12, etc preferably in terms of byp diode substrs),
%gammaPmp is the temperature coefficient of max power,
% reported in datasheet, VMaxSTCStrUnshaded is the unshaded Vmp of the string at STC,
% VStrMPPT is the lower and upper bounds of the inverter's MPPT range, and
% Shade DB is the database of shading losses (created by the DBX scripts at NREL)
*/
#include <functional>   // std::greater
#include <algorithm>    // std::sort
#include <math.h> // logarithm function

#include "core.h"
#include "lib_util.h"
#include "lib_pv_shade_loss_mpp.h"

static var_info _cm_vtab_pv_get_shade_loss_mpp[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT, SSC_ARRAY, "global_poa_irrad", "Global POA irradiance", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "diffuse_irrad", "Diffuse irradiance", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "str_shade_fracs", "Shading fractions for each string", "", "", "PV Shade Loss DB", "*", "", "" },

	// for cell temp correction and checking for global MPP value
	{ SSC_INPUT, SSC_ARRAY, "pv_cell_temp", "PV cell temperature", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "mods_per_string", "Modules per string", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "str_vmp_stc", "Unshaded Vmp of the string at STC", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "v_mppt_low", "Lower bound of inverter MPPT range", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "v_mppt_high", "Upper bound of inverter MPPT range", "", "", "PV Shade Loss DB", "*", "", "" },


	// testing indices from lookup
	{ SSC_OUTPUT, SSC_ARRAY, "N", "N", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "d", "d", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "t", "t", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "S", "S", "", "", "PV Shade Loss DB", "*", "", "" },


	/*  hourly or sub hourly shading for each string*/
	{ SSC_OUTPUT, SSC_ARRAY, "shade_loss", "Shade loss fraction", "", "", "PV Shade Loss DB", "*", "", "" },


var_info_invalid };

class cm_pv_get_shade_loss_mpp : public compute_module
{
public:

	cm_pv_get_shade_loss_mpp()
	{
		add_var_info(_cm_vtab_pv_get_shade_loss_mpp);
	}

	void exec() override
	{

		size_t nrec, count;
		ssc_number_t* global_poa_irrad = as_array("global_poa_irrad", &nrec);
		size_t step_per_hour = nrec / 8760;
		if (step_per_hour < 1 || step_per_hour > 60 || step_per_hour * 8760 != nrec)
			throw exec_error("pv_get_shade_loss_mpp", util::format("invalid number of global POA records (%d): must be an integer multiple of 8760", (int)nrec));
		//double ts_hour = 1.0 / step_per_hour;

		ssc_number_t* diffuse_irrad = as_array("diffuse_irrad", &count);
		if (count != nrec)
			throw exec_error("pv_get_shade_loss_mpp", util::format("invalid number of diffuse records (%d): must be equal to other input array sizes (%d)", (int)count, (int)nrec));

		util::matrix_t<double> str_shade_fracs = as_matrix("str_shade_fracs");
		count = str_shade_fracs.nrows();
		size_t num_strings = str_shade_fracs.ncols();
		if (count != nrec)
			throw exec_error("pv_get_shade_loss_mpp", util::format("invalid number of mocules per string records (%d): must be equal to other input array sizes (%d)", (int)count, (int)nrec));




// temperature correction 

		ssc_number_t* pv_cell_temp = as_array("pv_cell_temp", &count);
		if (count != nrec)
			throw exec_error("pv_get_shade_loss", util::format("invalid number of pv cell temp records (%d): must be equal to other input array sizes (%d)", (int)count, (int)nrec));

		ssc_number_t* mods_per_string = as_array("mods_per_string", &count);
		if (count != nrec)
			throw exec_error("pv_get_shade_loss", util::format("invalid number of modules per string records (%d): must be equal to other input array sizes (%d)", (int)count, (int)nrec));

		ssc_number_t* str_vmp_stc = as_array("str_vmp_stc", &count);
		if (count != nrec)
			throw exec_error("pv_get_shade_loss", util::format("invalid number of Vmp at STC records (%d): must be equal to other input array sizes (%d)", (int)count, (int)nrec));

		ssc_number_t* v_mppt_low = as_array("v_mppt_low", &count);
		if (count != nrec)
			throw exec_error("pv_get_shade_loss", util::format("invalid number of MPPT low records (%d): must be equal to other input array sizes (%d)", (int)count, (int)nrec));

		ssc_number_t* v_mppt_high = as_array("v_mppt_high", &count);
		if (count != nrec)
			throw exec_error("pv_get_shade_loss", util::format("invalid number of MPPT high records (%d): must be equal to other input array sizes (%d)", (int)count, (int)nrec));






// start here
		ssc_number_t *N = allocate("N", nrec);
		ssc_number_t *d = allocate("d", nrec);
		ssc_number_t *t = allocate("t", nrec);
		ssc_number_t *S = allocate("S", nrec);

		ssc_number_t *shade_loss = allocate("shade_loss", nrec);


		if (num_strings > 0)
		{
			ShadeDB8_mpp db8;
			db8.init();

			for (size_t irec = 0; irec < nrec; irec++)
			{
//				shade_loss[irec] = 1;
				shade_loss[irec] = 0;
				//				if (global_poa_irrad[irec] > 0)
//				{
					// shading fractions for each string
					std::vector<double> dbl_str_shade;
					for (size_t ins = 0; ins < num_strings; ins++)
						dbl_str_shade.push_back(str_shade_fracs.at(irec,ins));

					//Sort in descending order of shading
					std::sort(dbl_str_shade.begin(), dbl_str_shade.end(), std::greater<double>());
					//Need to round them to 10s (note should be integer)
					for (size_t i = 0; i < num_strings; i++)
						dbl_str_shade[i] /= 10.0;
					std::vector<int> str_shade;
					for (size_t i = 0; i < num_strings; i++)
						str_shade.push_back((int)round(dbl_str_shade[i]));
					//			str_shade.push_back((int)dbl_str_shade[i]);
					int s_max = -1; // = str_shade[0]
					int s_sum = 0; // = str_shade[0] that is if first element zero then sum should be zero
					for (size_t i = 0; i < num_strings; i++)
					{
						if (str_shade[i] > s_max) s_max = str_shade[i];
						s_sum += str_shade[i];
					}
					//Now get the indices for the DB
//					if (s_sum > 0)
					if ((s_sum > 0) && (global_poa_irrad[irec] > 0))
					{
						int diffuse_frac = (int)round(diffuse_irrad[irec]*10.0 / global_poa_irrad[irec] );
						if (diffuse_frac < 1) diffuse_frac = 1;
						int counter = 1;
						bool found = false;
						std::vector<int> cur_case;
						if (num_strings > 1)
						{
							counter = 0;
							for (int i2 = 0; i2 <= s_max; i2++)
							{
								if (num_strings == 2)
								{
									counter++;
									//std::vector<int> cur_case{ s_max, i2 };
									cur_case.clear();
									cur_case.push_back(s_max);
									cur_case.push_back(i2);
									if (str_shade == cur_case)
										found = true;
								}
								else
								{
									for (int i3 = 0; i3 <= i2; i3++)
									{
										if (num_strings == 3)
										{
											counter++;
											//std::vector<int> cur_case{ s_max, i2, i3 };
											cur_case.clear();
											cur_case.push_back(s_max);
											cur_case.push_back(i2);
											cur_case.push_back(i3);
											if (str_shade == cur_case)
												found = true;
										}
										else
										{
											for (int i4 = 0; i4 <= i3; i4++)
											{
												if (num_strings == 4)
												{
													counter++;
													//std::vector<int> cur_case{ s_max, i2, i3, i4 };
													cur_case.clear();
													cur_case.push_back(s_max);
													cur_case.push_back(i2);
													cur_case.push_back(i3);
													cur_case.push_back(i4);
													if (str_shade == cur_case)
														found = true;
												}
												else
												{
													for (int i5 = 0; i5 <= i4; i5++)
													{
														if (num_strings == 5)
														{
															counter++;
															//std::vector<int> cur_case{ s_max, i2, i3, i4, i5 };
															cur_case.clear();
															cur_case.push_back(s_max);
															cur_case.push_back(i2);
															cur_case.push_back(i3);
															cur_case.push_back(i4);
															cur_case.push_back(i5);
															if (str_shade == cur_case)
																found = true;
														}
														else
														{
															for (int i6 = 0; i6 <= i5; i6++)
															{
																if (num_strings == 6)
																{
																	counter++;
																	//std::vector<int> cur_case{ s_max, i2, i3, i4, i5, i6 };
																	cur_case.clear();
																	cur_case.push_back(s_max);
																	cur_case.push_back(i2);
																	cur_case.push_back(i3);
																	cur_case.push_back(i4);
																	cur_case.push_back(i5);
																	cur_case.push_back(i6);
																	if (str_shade == cur_case)
																		found = true;
																}
																else
																{
																	for (int i7 = 0; i7 <= i6; i7++)
																	{
																		if (num_strings == 7)
																		{
																			counter++;
																			//std::vector<int> cur_case{ s_max, i2, i3, i4, i5, i6, i7 };
																			cur_case.clear();
																			cur_case.push_back(s_max);
																			cur_case.push_back(i2);
																			cur_case.push_back(i3);
																			cur_case.push_back(i4);
																			cur_case.push_back(i5);
																			cur_case.push_back(i6);
																			cur_case.push_back(i7);
																			if (str_shade == cur_case)
																				found = true;
																		}
																		else
																		{
																			for (int i8 = 0; i8 <= i7; i8++)
																			{
																				if (num_strings == 8)
																				{
																					counter++;
																					//std::vector<int> cur_case{ s_max, i2, i3, i4, i5, i6, i7, i8 };
																					cur_case.clear();
																					cur_case.push_back(s_max);
																					cur_case.push_back(i2);
																					cur_case.push_back(i3);
																					cur_case.push_back(i4);
																					cur_case.push_back(i5);
																					cur_case.push_back(i6);
																					cur_case.push_back(i7);
																					cur_case.push_back(i8);
																					if (str_shade == cur_case)
																						found = true;
																				}
																				else
																				{
																					// error message or throw error
																					counter = 0;
																				}
																			} // for i7
																			if (found) break;
																		}
																	} // for i7
																	if (found) break;
																}
															} // for i6
															if (found) break;
														}
													} // for i5
													if (found) break;
												}
											} // for i4
											if (found) break;
										}
									} // for i3
									if (found) break;
								}
								if (found) break;
							} // for i2
						} // (num_strings > 1)


						N[irec] = (ssc_number_t)num_strings;
						d[irec] = (ssc_number_t)diffuse_frac;
						t[irec] = (ssc_number_t)s_max;
						S[irec] = (ssc_number_t)counter;

						std::vector<double>vmpp = db8.get_vector(num_strings, diffuse_frac, s_max, counter, ShadeDB8_mpp::VMPP);
						std::vector<double>impp = db8.get_vector(num_strings, diffuse_frac, s_max, counter, ShadeDB8_mpp::IMPP);
						double p_max_frac = 0;


						// temp correction and out of global MPP
						int p_max_ind = 0;
						std::vector<double> pmp_fracs;

						for (size_t i = 0; i < vmpp.size() && i < impp.size(); i++)
						{
							double pmp = vmpp[i] * impp[i];
							pmp_fracs.push_back(pmp);
							if (pmp > p_max_frac)
							{
								p_max_frac = pmp;
								p_max_ind = (int)i;
							}
						}

	
						// The global max power point is in range!
//						shade_loss[irec] = (ssc_number_t)(1 - p_max_frac);

						/*
						%Try scaling the voltages using the Sandia model.Taking numbers from
						%their database for the Yingli YL230.It's a similar module (mc-si,60 cell, etc)to the
						%Trina 250 PA05 which the database was build from.But user may need more
						%input into this!!!
						*/
						double n = 1.263;
						double BetaVmp = -0.137*mods_per_string[irec]; //mult by ModsPerString because it's in V
						double Ns = 60 * mods_per_string[irec]; //X modules, each with 60 cells
						double C2 = -0.05871;
						double C3 = 8.35334;
						double k = 1.38066E-23; //J / K, Boltzmann's constant
						double q = 1.60218E-19;  // Coulomb, elementary charge
						double Tc = pv_cell_temp[irec];
						double deltaTc = n*k*(Tc + 273.15) / q; //Thermal voltage
						double VMaxSTCStrUnshaded = str_vmp_stc[irec];
						double scale_g = global_poa_irrad[irec] / 1000.0;

						std::vector<double> TcVmps;

						for (size_t i = 0; i < vmpp.size(); i++)
							TcVmps.push_back(vmpp[i] * VMaxSTCStrUnshaded + C2*Ns*deltaTc*::log(scale_g) + C3*Ns*pow((deltaTc*::log(scale_g)), 2) + BetaVmp*(Tc - 25));
						/*
						%Now want to choose the point with a V in range and highest power
						%First, figure out which max power point gives lowest loss
						*/
						double Veemax = TcVmps[p_max_ind];
						if ((Veemax >= v_mppt_low[irec]) && (Veemax <= v_mppt_high[irec]))
							// The global max power point is in range!
							shade_loss[irec] = (ssc_number_t)(1 - p_max_frac);
						else
						{
							//	The global max power point is NOT in range
							double p_frac = 0;
							
							for (size_t i = 0; i < TcVmps.size() && i < pmp_fracs.size(); i++)
							{
								if ((TcVmps[i] >= v_mppt_low[irec]) && (TcVmps[i] <= v_mppt_high[irec]))
								{
									if (pmp_fracs[i] > p_frac)
										p_frac = pmp_fracs[i];
								}
							}
							
							shade_loss[irec] = (ssc_number_t)(1 - p_frac);
						}




					} //(sum >0)
					else // either shade frac sum = 0 or global = 0
					{
						if (s_sum <=0 ) // to match with Matlab results
							shade_loss[irec] = 0;
						else
							shade_loss[irec] = 0;
//						shade_loss[irec] = 1;
					}
//				} //(global > 0)
			}// for irec
		} //  (num_strings > 0) 
		else
		{
			log(util::format("no DB loaded num strings = %d", num_strings), SSC_WARNING);
		}

		/*
		  
			Veemax = TcVmps(Pmaxind);
		if and(VStrMPPT(1) <= Veemax, VStrMPPT(2) >= Veemax)
			% The global max power point is in range!
			ShadeLoss = 1 - PmaxFrac;
		elseif isempty(PFracs(and(TcVs >= VStrMPPT(1), TcVs <= VStrMPPT(2))))
			ShadeLoss = 1;
		else
			%    %The global max power point is NOT in range
			ShadeLoss = 1 - max(PFracs(and(TcVs >= VStrMPPT(1), TcVs <= VStrMPPT(2))));
		end
		*/

	}
};

DEFINE_MODULE_ENTRY(pv_get_shade_loss_mpp, "PV get shade loss fraction for strings", 1)
