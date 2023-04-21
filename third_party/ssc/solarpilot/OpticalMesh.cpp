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

#include <math.h>
#include <algorithm>
#include <stdio.h>

#include "OpticalMesh.h"
#include "exceptions.hpp"
#include "definitions.h"

using namespace std;

void LayoutData::set_data(double Extents_r[2], double Extents_az[2], double Tht, double Alpha, double Theta, double l_f, 
		double h_h, double h_w, double S_h, double W_rec, double F_tol, double T_res, bool Flat, bool Onslant, int Nph, int Npw)
{	
	for(int i=0; i<2; i++){
		extents_r[i] = Extents_r[i];
		extents_az[i] = Extents_az[i];
	}
	tht = Tht;
	alpha = Alpha;
	theta = Theta;
	L_f = l_f;
	H_h = h_h;
	H_w = h_w;
	s_h = S_h;
	w_rec = W_rec;
	f_tol = F_tol;
	t_res = T_res;
	flat = Flat;
	onslant = Onslant;
	nph = Nph;
	npw = Npw;
}

//-------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------

derivatives::derivatives(LayoutData &data)
{
	pi = PI;
        
    Data = data;
    //calculate the value of the coefficients
 //   double alpha = Data.alpha+pi/2.;
 //   double theta = Data.theta;
 //   if( Data.flat){
 //       c1 = cos(alpha)*cos(theta);
 //       c2 = sin(alpha)*cos(theta);
 //       c3 = sin(theta);
	//}

	//Calculate constants that are used during simulation
    lbase = sqrt(pow(Data.H_h/Data.nph,2) + pow(Data.H_w/Data.npw,2));
	tht2 = Data.tht*Data.tht;
	sqt2 = sqrt(2.);
    sqtpi = sqrt(pi);
}

double derivatives::int_eval(double r, double lf){
	double ls = sqrt(r*r + tht2);
	double lfmls = lf-ls;

	//Check whether the focal length is equal to the slant length. Handle separately.
	if(fabs(lfmls) < .1){
		//Use the formulation for only Gaussian error contributions.
		return -r*Data.w_rec/(sqt2*sqtpi*ls*ls*ls*Data.s_h)*exp(-Data.w_rec*Data.w_rec/(8.*ls*ls*Data.s_h*Data.s_h));
	}
	else
	{
		//Use the formulation including both image size and error distribution
		double D = 2.*sqt2*lf*ls*Data.s_h;
		double Dinv = 1./D;
		double N1 = lbase*lfmls+lf*Data.w_rec;
		double N2 = lbase*(ls-lf)+lf*Data.w_rec;
		double T1 = N1*Dinv;
		double T2 = N2*Dinv;
		return -r*lf/(2.*lbase*lfmls*lfmls*ls)*(2.*sqt2/sqtpi*lf*Data.s_h*(
			exp(-T2*T2)-exp(-T1*T1)) + Data.w_rec*(erf(T2) - erf(T1)) );
	}
}

vector<double> derivatives::d_eval(double r, double beta, double lf)
{
    /*
    Calculate the derivative of intercept factor as a function of position in 
    the field and certain optical parameters.
        
    r : Radial position
    beta : Elevation angle of the receiver aperture
    lf : Focal length of the heliostat at position r
    */

	double cos_theta = cos(Data.theta);
	double slant = sqrt(Data.tht*Data.tht + r*r);

	double dint_dr = int_eval(r, lf);

	vector<double> results;

	if(Data.flat){
		results.push_back(Data.tht*(Data.tht*cos_theta*cos(Data.alpha-beta)+r*sin(Data.theta))/(slant*slant*slant) + dint_dr);	//dr
		results.push_back(r*cos_theta*sin(Data.alpha-beta)/slant);	//dB
	}
	else{
		results.push_back(Data.tht*(Data.tht*cos_theta+r*sin(Data.theta))/(slant*slant*slant) + dint_dr);	//dr
		results.push_back(0.);	//dB
	}

	return results;

    //Calculate the elevation angle of the receiver to heliostat        
 //   double gamma = atan2(Data.tht,r);
 //   double slant = sqrt(Data.tht*Data.tht + r*r);
 //   double dgamma_dr = -Data.tht/(slant*slant);
 //       
 //   double dint_dr = int_eval(r, lf);
 //   

 //   if(Data.flat){
 //       double cos_gamma = cos(gamma);
 //       double sin_gamma = sin(gamma);
 //       double cos_beta = cos(beta);
 //       double sin_beta = sin(beta);
 //   
	//	vector<double> results;
	//	results.push_back(dgamma_dr*(c3*cos_gamma-sin_gamma)*(c1*cos_beta + c2*sin_beta) + dint_dr);	//dr
	//	results.push_back(cos_gamma*(c2*cos_beta - c1*sin_beta)/max(1.e-6,r));		//dB

 //       return results;
	//}
 //   else{
 //       //cylindrical
 //       double dr = dgamma_dr * sin(Data.theta - gamma);
	//	vector<double> results;
	//	results.push_back(dr + dint_dr);
	//	results.push_back(0.);
 //       return results;
	//}
}

//-------------------------------------------------------------------------------------------------
void tree_node::setup(tree_node *child0){
	terminal = false;
	m0 = child0;
	m1 = m0;
}
void tree_node::setup(tree_node *child0, tree_node *child1){
	terminal = false;
	m0 = child0;
	m1 = child1;
}
void tree_node::setup(vector<void*> Data){
	terminal = true;
	data = Data;
}
bool tree_node::is_terminal(){
	return terminal;
}
vector<void*> *tree_node::get_array(){
	return &data;
}
tree_node *tree_node::m_proc(string &key, int index){
	char c;
	try
	{
		c = key.at(index);
	}
	catch(...){
		return this;
	}
	if(c == 't' || terminal){	
		return this;
	}
	if(c == 'x' || c == '0'){	
		return m0->m_proc(key, index+1);
	}
	if(c == '1'){
		return m1->m_proc(key, index+1);
	}
	
	throw spexception("Invalid key index while parsing optical mesh.");
}
vector<tree_node*> tree_node::m_get_children(){
	vector<tree_node*> kids;
	if(! terminal){
		if( m0 == m1 ){
			kids.push_back(m0);
			vector<tree_node*> m0kids = m0->m_get_children();
			for(unsigned int i=0; i<m0kids.size(); i++)
				kids.push_back(m0kids.at(i));
		}
		else{
			kids.push_back(m0);
			kids.push_back(m1);
			vector<tree_node*> m0kids = m0->m_get_children();
			vector<tree_node*> m1kids = m1->m_get_children();
			for(unsigned int i=0; i<m0kids.size(); i++)
				kids.push_back(m0kids.at(i));
			for(unsigned int i=0; i<m1kids.size(); i++)
				kids.push_back(m1kids.at(i));
		}
	}
	return kids;
}
vector<void*> tree_node::get_child_data(){
	if(terminal){
		return data;
	}
	else{
		if(m0 == m1){
			return m0->get_child_data();
		}
		else{
			vector<void*> m0dat, m1dat, alldat;
			m0dat = m0->get_child_data();
			m1dat = m1->get_child_data();
			for(unsigned int i=0; i<m0dat.size(); i++)
				alldat.push_back(m0dat.at(i));
			for(unsigned int i=0; i<m1dat.size(); i++)
				alldat.push_back(m1dat.at(i));
			return alldat;
		}
	}

}

//-------------------------------------------------------------------------------------------------
void opt_element::set_range(double xrlo, double xrhi, double yrlo, double yrhi){
	xr[0] = xrlo;
	xr[1] = xrhi;
	yr[0] = yrlo;
	yr[1] = yrhi;
}

void opt_element::set_range(double xri[2], double yri[2]){	
	for(int i=0; i<2; i++){
		xr[i] = xri[i];
		yr[i] = yri[i];
	}
}

opt_element *opt_element::process(string &key, int index){
	return (opt_element*)m_proc(key, index);
}

vector<opt_element*> opt_element::get_children(){
	vector<opt_element*> children;
	vector<tree_node*> m_children = m_get_children();
	for( vector<tree_node*>::iterator it = m_children.begin(); it != m_children.end(); it++)
		children.push_back( (opt_element*) *it );
	return children;
}

double *opt_element::get_yr(){return yr;}
double *opt_element::get_xr(){return xr;}
//-------------------------------------------------------------------------------------------------
optical_hash_tree::optical_hash_tree()
{
	pi = PI;
	divs_updated = false;
	log2inv = 1./log(2.);
}

void optical_hash_tree::create_mesh(LayoutData *data){
	/*
	Create a mesh of the heliostat field according to the performance surface
	provided by the 'integrals' class.
	*/
	Data = data;
        
	//Calculate min and max recursion levels based on user zone size limitations
	double dextr = (Data->extents_r[1] - Data->extents_r[0]);
	max_rec_level_r = (int)floor( log(dextr/Data->min_zsize_r)*log2inv );
	min_rec_level_r = (int)ceil( log(dextr/Data->max_zsize_r)*log2inv );
	//Calculate coef. for azimuthal mesh
	double dexta = (Data->extents_az[1] - Data->extents_az[0]);
	max_rec_level_a_dr = dexta/Data->min_zsize_a;
	min_rec_level_a_dr = dexta/Data->max_zsize_a;

	//set up the derivatives class
	derivs = derivatives(*Data);
    
	//estimate the maximum number of nodes and reserve memory
	int nmaxdivr = (int)pow(2., max_rec_level_r);		//maximum number of zones radially
	int maxreclevela = (int)floor( log(max_rec_level_a_dr*(Data->extents_r[1]+Data->extents_r[0])*.5)*log2inv);  //max azimuthal recursion
	int nmaxdiva = (int)pow(2., maxreclevela);	//max azimuthal zones (estimate)
	int nmaxterm =  nmaxdivr * nmaxdiva;	//total max number of zones
	int maxreclevel = max(max_rec_level_r, maxreclevela); //worst case max recursion level
	int nmaxnodes = 0;
	for(int i=0; i<maxreclevel; i++)
		nmaxnodes += nmaxterm/(int)pow(2.,i);		//Add each level in the node tree
	
	//Try reserving the number of required nodes, catch any memory error
	try
	{
		nodes.reserve(nmaxnodes*2); //include a 100% buffer
	}
	catch(...)
	{
		char msg[200];
		sprintf(msg, "An error occurred while allocating memory for the optical mesh elements. This"
			" can occur when the field layout zone settings are configured incorrectly or when insufficient "
			"memory is available. Attempting %d nodes.", nmaxnodes);
		throw spexception( (const char*)msg );
	}
	
	//define a new head node
	//nodes.push_back(head_node);
        
	head_node.set_range(Data->extents_r[0], Data->extents_r[1], Data->extents_az[0], Data->extents_az[1]);
	create_node(head_node, true, 0, 0);

}
void optical_hash_tree::create_node(opt_element &node, bool rad_direction, int rec_level_r, int rec_level_a){
    //evaluate the derivatives at the center of the element.
    double 
		xr0 = node.get_xr()[0],
		xr1 = node.get_xr()[1],
		yr0 = node.get_yr()[0],
		yr1 = node.get_yr()[1],
		C0 = (xr0 + xr1)*0.5,
		C1 = (yr0 + yr1)*0.5;
        
	double Lf;
	if(Data->onslant){
        Lf = sqrt(C0*C0 + Data->tht*Data->tht);
	}
    else{
        Lf = Data->L_f;
	}
    double ddr,ddB;
	vector<double> res = derivs.d_eval(C0,C1,Lf);
	ddr = res.at(0);
	ddB = res.at(1);

	double dr = fabs(ddr*(xr1 - xr0));
    double dB = fabs(ddB*(yr1 - yr0));
        
	//Calculate the azimuthal zone size limits for this radius
	int max_rec_level_a = (int)floor( log2inv*log(C0*max_rec_level_a_dr) );
	int min_rec_level_a = (int)ceil( log2inv*log(C0*min_rec_level_a_dr) );
                
    if( rad_direction){
		double x_r_o[] = {C0,xr1};
		double x_r_i[] = {xr0,C0};
		double y_r[] = {yr0, yr1}; 

        if((dr > Data->f_tol || rec_level_r < min_rec_level_r) && (rec_level_r < max_rec_level_r)) {
            //self.__r_divs += 1  //keep track of the total number of divisions
            //print "rad divs",self.__r_divs,"dr/dB",dr,dB
                
            //Create new child nodes
            //outer node
            nodes.push_back(opt_element());
            opt_element *m1 = &nodes.back();
            m1->set_range(x_r_o, y_r );
            //inner node
            nodes.push_back(opt_element());
            opt_element *m0 = &nodes.back();
            m0->set_range(x_r_i, y_r );
    
            node.setup(m0, m1);
                
            create_node(*m0, ! rad_direction, rec_level_r + 1, rec_level_a);
            create_node(*m1, ! rad_direction, rec_level_r + 1, rec_level_a);
    
            return;
		}
		else if( (dB > Data->f_tol || rec_level_a < min_rec_level_a) && (rec_level_a < max_rec_level_a)){
            //Even though the split didn't occur, check whether a split will
            //occur in the other direction
            nodes.push_back(opt_element());
            opt_element *m0 = &nodes.back();
				
            m0->set_range(xr0, xr1, yr0, yr1);  //keep current range
    
            node.setup(m0);
    
            create_node(*m0, !rad_direction, rec_level_r, rec_level_a);
            return;
		}
        else{
			vector<void*> T;
            node.setup(T);
            return;
		}
	}
    else{
            
		double x_r[] = {xr0,xr1};
		double y_r_i[] = {yr0,C1}; 
		double y_r_o[] = {C1,yr1};
            
        if( (dB > Data->f_tol || rec_level_a < min_rec_level_a) && (rec_level_a < max_rec_level_a)){
            //Create new child nodes
            //outer node
            nodes.push_back(opt_element());
            opt_element *m1 = &nodes.back();
            m1->set_range(x_r, y_r_o );
            //inner node
            nodes.push_back(opt_element());
            opt_element *m0 = &nodes.back();
            m0->set_range(x_r, y_r_i );
    
            node.setup(m0, m1);
    
            create_node(*m0, !rad_direction, rec_level_r, rec_level_a + 1);
            create_node(*m1, !rad_direction, rec_level_r, rec_level_a + 1);
    
            return;
		}
		else if( (dr > Data->f_tol || rec_level_r < min_rec_level_r) && (rec_level_r < max_rec_level_r)){
            //Even though the split didn't occur, check whether a split will
            //occur in the other direction
            nodes.push_back(opt_element());
            opt_element *m0 = &nodes.back();
            m0->set_range(xr0, xr1, yr0, yr1); //keep current range
    
            node.setup(m0);
    
            create_node(*m0, !rad_direction, rec_level_r, rec_level_a);
            return;
		}
        else{
			vector<void*> T;
            node.setup(T);
            return;
		}
		return;
	}
}
void optical_hash_tree::update_divisions(double res){
    double *r = Data->extents_r; 
        
    //calculate the number of divisions required to achieve the required resolution
 	nr_req = int(ceil( log((r[1]-r[0])/res)/log(2.)));
    na_req = int(ceil( log((pi*(r[1]-r[0]))/res)/log(2.)));
    divs_updated = true;
}
string optical_hash_tree::pos_to_binary(double x, double y){
	return pos_to_binary(x, y, Data->t_res);
}
string optical_hash_tree::pos_to_binary(double x, double y, double res){
	/*
	Convert an x-y position into a binary tag
	*/
        
	if( ! divs_updated)
		update_divisions(res);
        
	string tag;
        
	double pr = sqrt(x*x + y*y);
	double paz = atan2(x,y);
	bool rad_mode = true; //start with radius
        
	double 
		az0 = Data->extents_az[0],
		az1 = Data->extents_az[1],
		r0 = Data->extents_r[0],
		r1 = Data->extents_r[1];
	    
	int nc = max(nr_req, na_req)*2;
        
	for(int i=0; i<nc; i++){
		if( rad_mode){
			double cr = (r0 + r1)*0.5;
			if(pr > cr){
				r0 = cr;
				tag.append("1");
			}
			else{
				r1 = cr;
				tag.append("0");
			}
		}
		else{
			double caz = (az0 + az1)*0.5;
			if(paz > caz){
				az0 = caz;
				tag.append("1");
			}
			else{
				az1 = caz;
				tag.append("0");
			}
		}
		rad_mode = ! rad_mode;
	}
	return tag;
}
void optical_hash_tree::add_object(void* object, double locx, double locy){
	add_object(object, locx, locy, Data->t_res);
}
void optical_hash_tree::add_object(void* object, double locx, double locy, double res){
	/*
    Take an object in cartesian coordinates and add it to the appropriate
    element in the mesh
    */
        
    string tag = pos_to_binary(locx, locy, res);
    opt_element *element = (opt_element*)head_node.process(tag,0);
    element->get_array()->push_back(object);  
}
void optical_hash_tree::reset(){
	Data = 0;
	head_node = opt_element();
	nodes.clear();
	divs_updated = false;
	nr_req = -1;
	na_req = -1;
}
vector<vector<void* >* > optical_hash_tree::get_terminal_data(){
	vector<vector<void* >* > retdata;
	for( vector<opt_element>::iterator it = nodes.begin(); it != nodes.end(); it++){
		if(! it->is_terminal() ) continue;
		retdata.push_back(it->get_array());
	}
	return retdata;
}
vector<opt_element*> optical_hash_tree::get_terminal_nodes(){
	vector<opt_element*> tnodes;

	for(int i=0; i<(int)nodes.size(); i++){
		if( nodes.at(i).is_terminal() )
			tnodes.push_back(&nodes.at(i));
	}

	return tnodes;
}
