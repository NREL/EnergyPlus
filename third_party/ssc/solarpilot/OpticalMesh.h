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

#ifndef _OPTICAL_MESH_
#define _OPTICAL_MESH_ 1

#include <vector>
#include <string>

struct LayoutData
{
    /*    
	'extents_r' = [rmin, rmax]  | Bounding radii for mesh region
	'extents_az'= [azccw, azcw] | Bounding azimuth angles for mesh region
	'tht'     = (float)         | Tower height
	'flat'    = (bool)          | Receiver is flat (cylindrical=False)
	'onslant' = (bool)          | Focal length == slant
	'alpha'   = (float)         | Receiver azimuth angle (rad)
	'theta'   = (float)         | Receiver elevation angle (rad)
	'f_tol'   = (float)         | Maximum function deviation within single node
	't_res'	  = (float)		    | Tag resolution
	'L_f'     = (float)         | heliostat focal length
	'H_h'     = (float)         | Heliostat height
	'H_w'     = (float)         | Heliostat width
	'nph'     = (int)           | Number of cant panels vertically
	'npw'     = (int)           | Number of cant panels horizontally
	's_h'     = (float)         | Reflected beam error distribution [rad]
	'w_rec'   = (float)         | Receiver width
	'max_zsize_r' = (float)		| Maximum optical zone size (radial)
	'max_zsize_a' = (float)		| Maximum optical zone size (azimuthal)
	'min_zsize_r' = (float)		| Minimum optical zone size (radial)
	'min_zsize_a' = (float)		| Minimum optical zone size (azimuthal)
	*/
	double extents_r[2];
	double extents_az[2];
	double
		tht,
		alpha, 
		theta,
		L_f, 
		H_h,
		H_w, 
		s_h,
		w_rec,
		f_tol,
		t_res,
		max_zsize_r,
		max_zsize_a,
		min_zsize_r,
		min_zsize_a;
	bool
		flat,
		onslant;
	int 
		nph,
		npw;
	void set_data(double Extents_r[2], double Extents_az[2], double Tht, double Alpha, double Theta, double l_f, 
		double h_h, double h_w, double S_h, double W_rec, double F_tol, double T_res, bool Flat, bool Onslant, 
		int Nph, int Npw);
};
//-------------------------------------------------------------------------------------------------

class derivatives
{
	double 
		lbase,
		c1, c2, c3,
		tht2, sqt2, sqtpi;
	double pi;
	LayoutData Data;
	
public:
	derivatives(){};
	derivatives(LayoutData &data);
	double int_eval(double r, double lf);
	std::vector<double> d_eval(double r, double beta, double lf);
};

//-------------------------------------------------------------------------------------------------

class tree_node
{
	tree_node *m0, *m1;
	std::vector<void*> data;
	bool terminal;
	
protected:
	tree_node *m_proc(std::string &key, int index);
	std::vector<tree_node*> m_get_children();
	
public:
	void setup(tree_node *child0);
	void setup(tree_node *child0, tree_node *child1);
	void setup(std::vector<void*> data);
	bool is_terminal();
	std::vector<void*> *get_array();
	std::vector<void*> get_child_data();

};

//-------------------------------------------------------------------------------------------------

class opt_element : public tree_node
{
	double
		xr[2],
		yr[2];
public:
	opt_element(){};
	void set_range(double xrlo, double xrhi, double yrlo, double yrhi);
	void set_range(double xr[2], double yr[2]);
	double *get_yr();
	double *get_xr();
	opt_element *process(std::string &key, int index);
	std::vector<opt_element*> get_children();
};

//-------------------------------------------------------------------------------------------------

class optical_hash_tree
{
	LayoutData *Data;
	std::vector<opt_element> nodes;
	derivatives derivs;
	opt_element head_node;
	bool divs_updated;
	int nr_req, na_req; //The number of divisions required to achieve the desired resolution
	int min_rec_level_r, max_rec_level_r;
	double min_rec_level_a_dr, max_rec_level_a_dr;
	double log2inv;
	double pi;
protected:
	void create_node(opt_element &node, bool rad_direction, int rec_level_r, int rec_level_a);
	void update_divisions(double res);

public:
	optical_hash_tree();
	void reset();
	void create_mesh(LayoutData *Data);
	void add_object(void *object, double locx, double locy);
	void add_object(void *object, double locx, double locy, double res);
	std::string pos_to_binary(double x, double y);
	std::string pos_to_binary(double x, double y, double res);
	std::vector<std::vector<void*>*> get_terminal_data();
	std::vector<opt_element*> get_terminal_nodes();
	
};


#endif
