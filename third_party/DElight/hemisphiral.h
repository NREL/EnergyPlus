// hemisphiral.h
//
/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: R.J. Hitchcock and W.L. Carroll
 *           Building Technologies Department
 *           Lawrence Berkeley National Laboratory
 */

// This work was supported by the Assistant Secretary for Energy Efficiency
// and Renewable Energy, Office of Building Technologies,
// Building Systems and Materials Division of the
// U.S. Department of Energy under Contract No. DE-AC03-76SF00098.

/*
NOTICE: The Government is granted for itself and others acting on its behalf
a paid-up, nonexclusive, irrevocable worldwide license in this data to reproduce,
prepare derivative works, and perform publicly and display publicly.
Beginning five (5) years after (date permission to assert copyright was obtained),
subject to two possible five year renewals, the Government is granted for itself
and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
license in this data to reproduce, prepare derivative works, distribute copies to
the public, perform publicly and display publicly, and to permit others to do so.
NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL
LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY
INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE
WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
*/
struct	nearestdata
{
	int		indx;	//	index
	double	adist;	//	arcdist

};

struct HemiSphiral
{
	vector<Double>	valList;
	int		N;
	Double	zMin;	//	zMax always= +1; zMin=0 -> hemisphere; zMin=-1 -> full sphere
	Double	deltaz;
	Double	omega;
	Double	DA;
	Double	pitch;

	HemiSphiral();
	HemiSphiral(Double);
	HemiSphiral(int);
	HemiSphiral(Double, int);
	HemiSphiral(Double, vector<Double>&);
	void	init();

    Double&        operator [] (int ii);
    const Double&  operator [] (int ii) const;
	int		size();
	void	resize(int nsize);
	Double	valMax();
	Double	valMin();
	int		valMaxIndx();
	int		valMinIndx();
	Double	costheta(int ii);
	Double	theta(int);
	Double	phi(int);
	Double	phiMod2pi(int);
	Double	SLcum(int);
	Double	turnsTot();
	Double	phiTot();
	Double	SLTot();
	Double	x2D(int);
	Double	y2D(int);
	BGL::vector3	dir(int);

    HemiSphiral&	operator += (const HemiSphiral&	hs);
    HemiSphiral&	operator -= (const HemiSphiral&	hs);
    HemiSphiral&	operator *= (Double s);
    HemiSphiral&	operator /= (Double s);
    HemiSphiral		operator + (const HemiSphiral&	hs) const;
    HemiSphiral		operator - (const HemiSphiral&	hs) const;
    HemiSphiral		operator - () const;                // -hs
    HemiSphiral		operator * (Double s) const;          // hs * s
    HemiSphiral		operator / (Double s) const;          // hs / s

    HemiSphiral		reflect(bool b0, bool b1, bool b2);

	vector<int>		nearest4(Double, Double);
	vector<int>		nearest4(BGL::vector3);
	vector<int>		nearestn(int,BGL::vector3);
	int				nearestc(Double admax, BGL::vector3 dirext, vector<struct nearestdata>& nd);
	int				rowsearch(int jjstart, Double admax, BGL::vector3 dirext, vector<int>&, vector<Double>&);
//	vector<Double>	interpwgts(BGL::vector3 dirext, vector<struct nearestdata>& nd);
	vector<Double>	interpwgts(BGL::vector3);
	Double			interp(BGL::vector3);
	Double			interp(Double, Double);

	Double			TotIllum();
	Double			TotPlanarIllum(BGL::vector3 dir =BGL::vector3(0,0,1));
	Double			TotHorizIllum();

	void		summary();
	void		pointdump();
	ofstream&	pointdumpT21(ofstream&);
	void		plotarray(vector<vector<Double> >& PlotArray, BGL::RHCoordSys3 LCS);
	void		plotview(int =60, Double valMin =-99., Double valMax =-99., Double theta =0, Double phi =0, Double zeta =0);
	ofstream&	plotfile(ofstream&, int, Double theta =0, Double phi =0, Double zeta =0);
	ifstream&	load(ifstream&);
	ofstream&	save(ofstream&);
};

inline Double& HemiSphiral::operator [] (int ii)
{
    return(valList[ii]);
}

inline const Double& HemiSphiral::operator [] (int ii) const
{
    return(valList[ii]);
}

Double	arcdist(BGL::vector3 dir1, BGL::vector3 dir2);

vector<Double>	interpwgts(vector<struct nearestdata>& nd);
//vector<Double>	interpwgts(BGL::vector3);
