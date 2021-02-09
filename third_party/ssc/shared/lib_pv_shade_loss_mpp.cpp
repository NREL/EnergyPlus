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

#include "lib_pv_shade_loss_mpp.h"
#include <functional>   // std::greater
#include <algorithm>    // std::sort
#include <math.h> // logarithm function
#include <cstring> // memcpy

#include "lib_miniz.h" // decompression
#include "DB8_vmpp_impp_uint8_bin.h" // char* of binary compressed file

// define the following to use ssc message formatting 
#include <sstream>
// uncomment following define if shading database validation outputs desired
//#define SHADE_DB_DEBUG

typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned int uint;

short ShadeDB8_mpp::get_vmpp(size_t i)
{
	if (i < 6045840) // uint16 check
		return (short)((p_vmpp[2 * i + 1] << 8) | p_vmpp[2 * i]); 
	else 
		return -1;
};

short ShadeDB8_mpp::get_impp(size_t i)
{ 
	if (i < 6045840) // uint16 check
		return (short)((p_impp[2 * i + 1] << 8) | p_impp[2 * i]); 
	else 
		return -1; 
};


bool ShadeDB8_mpp::get_index(const size_t &N, const size_t &d, const  size_t &t, const size_t &S, const  db_type &DB_TYPE, size_t* ret_ndx)
{
	bool ret_val = false;
	//size_t ret_ndx=-1;
	size_t length=0;
//	size_t length_t =10, length_d=10;
	size_t iN = 0, id = 0, it = 0;

	// ret_ndx==0 is an error condition.
	// check N
	if ((N < 1) || (N>8)) return ret_val;
	// check d
	if ((d < 1) || (d>10)) return ret_val;
	// check t
	if ((t < 1) || (t>10)) return ret_val;

	// check S value for validity
	// find number of s vectors
	size_t size_s = n_choose_k(t + N - 1, t);
	if ((S < 1) || (S>size_s)) return ret_val;



	switch (DB_TYPE)
	{
		case VMPP:
			length = 8;
			break;
		case IMPP:
			length = 8;
			break;
	}
	if (length == 0) return ret_val;
	*ret_ndx = 0; // independent vectors for vmpp,impp,vs and is so offset=0

	size_t t_ub = 11; // upper bound of t index for iteration
	size_t d_ub = 10; // upper bound of d index for iteration
	do
	{
		iN++;
		d_ub = ((iN == N) ? d : 10);
		id = 0;
		do
		{
			id++;
			t_ub = (((iN==N) && (id==d)) ? t : 11);
			for (it = 1; it < t_ub; it++)
			{
				// find number of s vectors
				size_s = n_choose_k(it + iN - 1, it);
				// multiply by length of each S vector
				*ret_ndx += size_s*length;
			}
		} while (id < d_ub);
	} while (iN < N);
	*ret_ndx += (S - 1)*length;
	ret_val = true;
	return ret_val;
}

size_t ShadeDB8_mpp::n_choose_k(size_t n, size_t k)
{
	if (k > n) return 0;
	if (k * 2 > n) k = n - k;
	if (k == 0) return 1;

	size_t result = n;
	for (size_t i = 2; i <= k; ++i) {
		result *= (n - i + 1);
		result /= i;
	}
	return result;
}

std::vector<double> ShadeDB8_mpp::get_vector(const size_t &N, const size_t &d, const size_t &t, const size_t &S, const db_type &DB_TYPE)
{
	std::vector<double> ret_vec;
	size_t length = 0;
	switch (DB_TYPE)
	{
	case VMPP:
		length = 8;
		break;
	case IMPP:
		length = 8;
		break;
	}
	if (length == 0) return ret_vec;
	size_t ndx;
	if (get_index(N, d, t, S, DB_TYPE, &ndx))
	{
		for (size_t i = 0; i < length; i++)
		{ // could replace with single get!
			if (DB_TYPE == VMPP)
				ret_vec.push_back((double)get_vmpp(ndx + i) / 1000.0);
			else if (DB_TYPE == IMPP)
				ret_vec.push_back((double)get_impp(ndx + i) / 1000.0);
		}
	}
	return ret_vec;
}

void ShadeDB8_mpp::init()
{
	p_error_msg = "";
	p_warning_msg = "";
	p_vmpp_uint8_size = 12091680; // uint8 size from matlab
	p_impp_uint8_size = 12091680; // uint8 size from matlab
	p_vmpp = (unsigned char *)malloc(p_vmpp_uint8_size);//malloc(12091680); uint8 size
	p_impp = (unsigned char *)malloc(p_impp_uint8_size);//malloc(12091680); uint8 size
	p_compressed_size = 3133517; // from modified example5.c in miniz project
	decompress_file_to_uint8();
}

ShadeDB8_mpp::~ShadeDB8_mpp()
{
	if (p_vmpp)
		free(p_vmpp);
	if (p_impp)
		free(p_impp);
}



bool ShadeDB8_mpp::decompress_file_to_uint8()
{
	size_t status;
	uint8 *pTmp_data;

	size_t mem_size = p_vmpp_uint8_size + p_impp_uint8_size;

	pTmp_data = (uint8 *)malloc(mem_size);

	status = tinfl_decompress_mem_to_mem((void *)pTmp_data, mem_size, pCmp_data, p_compressed_size, TINFL_FLAG_PARSE_ZLIB_HEADER);

	memcpy(p_vmpp, pTmp_data, p_vmpp_uint8_size);
	memcpy(p_impp, pTmp_data + p_vmpp_uint8_size, p_impp_uint8_size);

	free(pTmp_data);

	if (status == TINFL_DECOMPRESS_MEM_TO_MEM_FAILED)
	{
		std::stringstream outm;
		outm << "tinfl_decompress_mem_to_mem() failed with status " << (int)status;
		p_error_msg = outm.str();
		return EXIT_FAILURE;
	}

	return true;
};

double ShadeDB8_mpp::get_shade_loss(double &gpoa, double &dpoa, std::vector<double> &shade_frac, bool use_pv_cell_temp, double pv_cell_temp, int mods_per_str, double str_vmp_stc, double mppt_lo, double mppt_hi)
{
	double shade_loss = 0;
	// shading fractions for each string
	size_t num_strings = shade_frac.size();
	// check for valid DB values
	if (dpoa > gpoa)
		dpoa = gpoa;
	if (num_strings > 0)
	{
		//Sort in descending order of shading
		std::sort(shade_frac.begin(), shade_frac.end(), std::greater<double>());
		//Need to round them to 10s (note should be integer)
		for (size_t i = 0; i < num_strings; i++)
			shade_frac[i] /= 10.0;
		std::vector<int> str_shade;
		for (size_t i = 0; i < num_strings; i++)
			str_shade.push_back((int)round(shade_frac[i]));
		int s_max = -1; // = str_shade[0]
		int s_sum = 0; // = str_shade[0] that is if first element zero then sum should be zero
		for (size_t i = 0; i < num_strings; i++)
		{
			if (str_shade[i] > s_max) s_max = str_shade[i];
			s_sum += str_shade[i];
		}
		//Now get the indices for the DB
		if ((s_sum > 0) && (gpoa > 0))
		{
			int diffuse_frac = (int)round(dpoa * 10.0 / gpoa);
			if (diffuse_frac < 1) diffuse_frac = 1;
			int counter = 1;
			bool found = false;
			if (num_strings > 1)
			{
				counter = 0;
				for (int i2 = 0; i2 <= s_max; i2++)
				{
					if (num_strings == 2)
					{
						counter++;
						std::vector<int> cur_case; // not on OS X { s_max, i2 };
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
								std::vector<int> cur_case; //{ s_max, i2, i3 };
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
										std::vector<int> cur_case;// { s_max, i2, i3, i4 };
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
												std::vector<int> cur_case;// { s_max, i2, i3, i4, i5 };
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
														std::vector<int> cur_case;// { s_max, i2, i3, i4, i5, i6 };
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
																std::vector<int> cur_case;// { s_max, i2, i3, i4, i5, i6, i7 };
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
																		std::vector<int> cur_case; // { s_max, i2, i3, i4, i5, i6, i7, i8 };
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



			std::vector<double>vmpp = get_vector(num_strings, diffuse_frac, s_max, counter, ShadeDB8_mpp::VMPP);
			std::vector<double>impp = get_vector(num_strings, diffuse_frac, s_max, counter, ShadeDB8_mpp::IMPP);
			double p_max_frac = 0;

			// temp correction and out of global MPP
			int p_max_ind = 0;
			std::vector<double> pmp_fracs;

			for (size_t i = 0; i < vmpp.size() && i < impp.size(); i++)
			{
				double pmp = vmpp[i] * impp[i];
				if (use_pv_cell_temp) pmp_fracs.push_back(pmp);
				if (pmp > p_max_frac)
				{
					p_max_frac = pmp;
					if (use_pv_cell_temp) p_max_ind = (int)i;
				}
			}

			if (use_pv_cell_temp)
			{
				/*
				%Try scaling the voltages using the Sandia model.Taking numbers from
				%their database for the Yingli YL230.It's a similar module (mc-si,60 cell, etc)to the
				%Trina 250 PA05 which the database was build from.But user may need more
				%input into this!!!
				*/
				double n = 1.263;
				double BetaVmp = -0.137*mods_per_str; //mult by ModsPerString because it's in V
				double Ns = 60 * mods_per_str; //X modules, each with 60 cells
				double C2 = -0.05871;
				double C3 = 8.35334;
				double k = 1.38066E-23; //J / K, Boltzmann's constant
				double q = 1.60218E-19;  // Coulomb, elementary charge
				double Tc = pv_cell_temp;
				double deltaTc = n*k*(Tc + 273.15) / q; //Thermal voltage
				double VMaxSTCStrUnshaded = str_vmp_stc;
				double scale_g = gpoa / 1000.0;
//				double TcVmpMax = vmpp[p_max_ind] * VMaxSTCStrUnshaded + C2*Ns*deltaTc*::log(scale_g) + C3*Ns*pow((deltaTc*::log(scale_g)), 2) + BetaVmp*(Tc - 25);
//				double TcVmpScale = TcVmpMax / vmpp[p_max_ind] / VMaxSTCStrUnshaded;

				std::vector<double> TcVmps;

				for (size_t i = 0; i < vmpp.size(); i++)
					TcVmps.push_back(vmpp[i] * VMaxSTCStrUnshaded + C2*Ns*deltaTc*::log(scale_g) + C3*Ns*pow((deltaTc*::log(scale_g)), 2) + BetaVmp*(Tc - 25));
				/*
				%Now want to choose the point with a V in range and highest power
				%First, figure out which max power point gives lowest loss
				*/
				double Veemax = TcVmps[p_max_ind];
				if ((Veemax >= mppt_lo) && (Veemax <= mppt_hi))
					// The global max power point is in range!
					shade_loss = 1.0 - p_max_frac;
				else
				{
					//	The global max power point is NOT in range
					double p_frac = 0;
					for (size_t i = 0; i < TcVmps.size() && i < pmp_fracs.size(); i++)
					{
						if ((TcVmps[i] >= mppt_lo) && (TcVmps[i] <= mppt_hi))
						{
							if (pmp_fracs[i] > p_frac)
								p_frac = pmp_fracs[i];
						}
					}

					shade_loss = 1.0 - p_frac;
				}


#ifdef SHADE_DB_DEBUG
				std::stringstream outm;
				outm << "\ni,Vmpp,Impp,pmp_fracs,TcVmps\n";
				for (size_t i = 0; i < TcVmps.size() && i < pmp_fracs.size(); i++)
				{
					outm << i << "," << vmpp[i] << "," << impp[i] << "," << pmp_fracs[i] << "," << TcVmps[i] << "\n";
				}
				outm << "\nshade loss = " << shade_loss << "\n";
				p_warning_msg = outm.str();
#endif

			}
			else // assume global max power point
			{
				shade_loss = 1.0 - p_max_frac;
			}

		} //(sum >0)
		else // either shade frac sum = 0 or global = 0
		{
			if (s_sum <= 0) // to match with Matlab results
				shade_loss = 0.0;
			else
				shade_loss = 0.0;
#ifdef SHADE_DB_DEBUG
			std::stringstream outm;
			outm << "\nglobal = " << gpoa << " and shade fraction = " << s_sum << " and shade loss = " << shade_loss << "\n";
			p_warning_msg = outm.str();
#endif
		}
	}
	return shade_loss;
}
