// btdf.cpp
//
/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: R.J. Hitchcock and W.L. Carroll
 *           Building Technologies Department
 *           Lawrence Berkeley National Laboratory
 */
/**************************************************************
 * C Language Implementation of DOE2.1d and Superlite 3.0
 * Daylighting Algorithms with new Complex Fenestration System
 * analysis algorithms.
 *
 * The original DOE2 daylighting algorithms and implementation
 * in FORTRAN were developed by F.C. Winkelmann at the
 * Lawrence Berkeley National Laboratory.
 *
 * The original Superlite algorithms and implementation in FORTRAN
 * were developed by Michael Modest and Jong-Jin Kim
 * under contract with Lawrence Berkeley National Laboratory.
 **************************************************************/

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
#pragma warning(disable:4786)

#include <stdlib.h>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <cstring>
#include <vector>
#include <limits>
using namespace std;

// BGLincludes
#include "BGL.h"
namespace BGL = BldgGeomLib;

#include "helpers.h"
#include "hemisphiral.h"
#include "btdf.h"

// writewndo() Error handler include
#include "DElightManagerC.h"

btdf::btdf()
{ }

btdf::btdf(int inM, int outN)
: HSoutList(inM)
{
	for (int ii=0; ii<inM; ii++) HSoutList[ii] = HemiSphiral(outN);
}

BGL::vector3	btdf::outDir(int ii)
{
	return HSoutList[0].dir(ii);
}


void btdf::summary()
{
	cout << "\noutput Hemisphirals:\n";
	HSoutList[0].summary();
}

void btdf::plotview(int first, int last, int viewsize, Double theta, Double phi, Double zeta)
{
	if (first < 0 || last < 0) return;
	if (last > (int)HSoutList.size() - 1) last = HSoutList.size() - 1;
	for (int ii=first; ii<=last; ii++) {
//		Double	phi2 = HSin.phiMod2pi(ii)*180/PI;
//		Double	turns = HSin.phi(ii)/(2*PI);
//		Double	thetain = HSin.theta(ii)*180/PI;
//		cout << "HSout[" << ii << "]: phi0: " << phi2 << " theta0: " << thetain << " turns: " << turns << "\n";
		cout << "HSout[" << ii << "]: \n";
		HSoutList[ii].plotview(viewsize, HSoutList[ii].valMin(), HSoutList[ii].valMax(), theta, phi, zeta);
	}
}


ifstream&	btdf::load(ifstream& infile)
{
	//	load HSoutList in base class
	int		ii = 0;
	HemiSphiral hs0;
	while (hs0.load(infile)) {
		HSoutList.push_back(hs0);
		ii += 1;
	}
	if (ii == 0) {
    	infile.clear(ios::failbit);
	    writewndo("btdf::load: HSoutList empty\n","e");
	    return(infile);
	}
    return(infile);
}

//	NOTE: btdfLoad is NOT a btdf member function
btdf* btdfLoad(ifstream& infile)
{
	//		read and parse 1st input line
	string			inlinestr;
	vector<string>	argList;
	getline(infile,inlinestr);
	argList = vParseList(inlinestr,",");
//	cout << "btdfLoad:\n";
//	for (int ii=0; ii<argList.size(); ii++) {
//		cout << ii << " " << argList[ii] << "\n";
//	}

	//	need to determine derived btdf class type from 1st line in file
	string	type = argList[0];
	if (type == "HS")	{
		btdfHS*	pbtdf0 = new btdfHS;
		pbtdf0->btdftype = type;
		//	load HSoutList in base class
		pbtdf0->load(infile);
		//	init HSin
		pbtdf0->HSin = HemiSphiral(pbtdf0->size());
		pbtdf0->HSin.init();
		return pbtdf0;
	}
	else if (type == "TRGZ")	{
		btdfTrgz*	pbtdf0 = new btdfTrgz;
		pbtdf0->btdftype = type;
		int	nTrgz = atoi(argList[1].c_str());
		if (nTrgz != pbtdf0->Trgz0.NTrgz()) {
			string errmsg = "btdf::load: infile nTrgz <-> pbtdf0->NTrgz() mismatch\n";
			writewndo(errmsg.c_str(),"e");
			return(0);
		}
		pbtdf0->Isym = atoi(argList[2].c_str());
		//	load DataIndx
		getline(infile,inlinestr);
		argList = vParseList(inlinestr,",");
//		cout << "btdfLoad: type: " << type << " Isym: " << pbtdf0->Isym << " argList.size(): " << argList.size() << " DataIndx.size(): " << pbtdf0->DataIndx.size() << "\n";
		for (int ii=0; ii<pbtdf0->Trgz0.NTrgz(); ii++) {
			pbtdf0->DataIndx[ii] = atoi(argList[ii].c_str());
//			cout << "(" << argList[ii].c_str() << " " << atoi(argList[ii].c_str()) << " " << pbtdf0->DataIndx[ii] << ") ";
		}
//		cout << "\n";
//		cout << " DataIndx.size(): " << pbtdf0->DataIndx.size() << "\n";
		//	load HSoutList in base class
		pbtdf0->load(infile);
		return pbtdf0;
	}
	else {
		string errmsg = "btdf::load: Bad btdf type: " + type + "\n";
	    writewndo(errmsg.c_str(),"e");
	    return(0);
	}
}

ofstream& btdf::save(ofstream& outfile)
{
	for(int ii=0; ii<(int)HSoutList.size(); ii++) {
		HSoutList[ii].save(outfile);
	}
	return outfile;
}

//	btdfHS
btdfHS::btdfHS()
{ }

btdfHS::btdfHS(int inM, int outN)
: btdf(inM,outN), HSin(inM)
{
	HSin.init();
}

HemiSphiral&        btdfHS::operator [] (int ii)
{
    return(HSoutList[ii]);
}

BGL::vector3	btdfHS::inDir(int ii)
{
	return HSin.dir(ii);
}

Double	btdfHS::qinterp(BGL::vector3 indir, BGL::vector3 outdir)
{
	BGL::point2	p0, p2D;

//	cout << "BTDF.interp:\n";
//	cout << "input angles\n";
//	vector<int>		nnin = HSin.nearestc(2.0*HSin.DA,indir);
	vector<struct nearestdata> nd;
	HSin.nearestc(2.0*HSin.DA,indir, nd);
	vector<Double>	inwgt = interpwgts(nd);
	//	final interpolation
	Double	interpVal = 0;
	for (int ii=0; ii<(int)inwgt.size(); ii++) {
		interpVal += inwgt[ii]*HSoutList[nd[ii].indx].interp(outdir);
	}
	return interpVal;

}

Double	btdfHS::qexact(int iin, int jout)
{
	return HSoutList[iin][jout];
}

void btdfHS::summary()
{
	cout << "BTDF summary\n";
	cout << "type: " << type() << " " << size() << "\n";
	cout << "input HemiSphiral:\n";
	HSin.summary();
	btdf::summary();
	/*
	for (int ii=0; ii<size(); ii++) {
		cout << "HSout[" << ii << "] ";
		cout << left;
		cout << setw(14) << HSoutList[ii].valMin();
		cout << setw(14) << HSoutList[ii].valMax();
		cout << setw(14) << HSoutList[ii].TotIllum();
		cout << "\n";
	}
	*/
}

ofstream& btdfHS::save(ofstream& outfile)
{
	outfile << "HS" << "\n";
	btdf::save(outfile);
	return outfile;
}


//	btdfTrgz
btdfTrgz::btdfTrgz()
: btdf(), Isym(0)
{
	DataIndx = vector<int>(Trgz0.NTrgz(),-1);
}

int	btdfTrgz::iiFindDataIndx(int iiTrgz)
{
	return DataIndx[iiTrgz];
}

HemiSphiral&        btdfTrgz::operator [] (int ii)
{
    return(HSoutList[iiFindDataIndx(ii)]);
}

BGL::vector3	btdfTrgz::inDir(int ii)
{
	return Trgz0.dir(ii);
}

Double	btdfTrgz::qinterp(BGL::vector3 indir, BGL::vector3 outdir)
{
	BGL::point2	p0, p2D;

//	cout << "BTDF.interp:\n";
//	cout << "input angles\n";
//	vector<int>		nnin = HSin.nearestc(2.0*HSin.DA,indir);
	vector<struct nearestdata> nd;
	Double	DA = sqrt(2*PI/size());
	Trgz0.nearestc(2.0*DA,indir, nd);
	vector<Double>	inwgt = ::interpwgts(nd);
	//	final interpolation
	Double	interpVal = 0;
	for (int ii=0; ii<(int)inwgt.size(); ii++) {
		interpVal += inwgt[ii]*HSoutList[nd[ii].indx].interp(outdir);
	}
	return interpVal;

}

Double	btdfTrgz::qexact(int iin, int jout)
{
	return HSoutList[DataIndx[iin]][jout];
}

void btdfTrgz::summary()
{
	cout << "BTDF summary\n";
	cout << "type: " << type() << " " << size() << "\n";
	cout << "Isym: " << Isym << "\n";
	Trgz0.summary();
	cout << "input data dirs: " << HSoutList.size() << "\n";
	cout << "DataIndx: ";
	for (int ii=0; ii<size(); ii++) {
		cout << DataIndx[ii] << " ";
	}
	cout << "\n";
	btdf::summary();
	/*
	for (int ii=0; ii<size(); ii++) {
		cout << "HSout[" << ii << "] ";
		cout << left;
		cout << setw(14) << HSoutList[ii].valMin();
		cout << setw(14) << HSoutList[ii].valMax();
		cout << setw(14) << HSoutList[ii].TotIllum();
		cout << "\n";
	}
	*/
}

ofstream& btdfTrgz::save(ofstream& outfile)
{
	outfile << "TRGZ," << Trgz0.NTrgz() << "," << Isym << "\n";
	for (int ii=0; ii<(int)DataIndx.size(); ii++) {
		outfile << DataIndx[ii];
		if ( ii < (int)DataIndx.size()-1 ) outfile << ',';
	}
	outfile << '\n';
	btdf::save(outfile);
	return outfile;
}



//	Tregenza
Tregenza::Tregenza()
: NTheta(8), deltaTheta(90./(NTheta-0.5))
{
	//	table lookups
	MPhi.resize(NTheta);
	MPhi[0] = 1;
	MPhi[1] = 6;
	MPhi[2] = 12;
	MPhi[3] = 18;
	MPhi[4] = MPhi[5] = 24;
	MPhi[6] = MPhi[7] = 30;

	omega.resize(NTheta);
	omega[0] = 0.0344;
	omega[1] = 0.0455;
	omega[2] = 0.0445;
	omega[3] = 0.0429;
	omega[4] = 0.0407;
	omega[5] = 0.0474;
	omega[6] = 0.0416;
	omega[7] = 0.0435;
}

Tregenza::~Tregenza()
{ }

double	Tregenza::PhiSym(double phi, int Isym)
{
	//	no symmetry
	if (Isym == 0) {
		return phi;
	}
	//	total rotational symmetry
	else if (Isym == 1) {
		return 0;
	}
	//	symmetry across phi = 0 - 180 deg
	else if (Isym == 2) {
		if (phi >= 0 && phi <= 180) return phi;	//	data points
		else return fmod(360 - phi,360);	//	reflect across symmetry axis
	}
	//	symmetry across phi = 90 - 270 deg
	else if (Isym == 3) {
		if ((phi >= 270 && phi < 360) || (phi >= 0 && phi <= 90) ) return phi;	//	data points
		else {
			if ((phi > 90 && phi < 180)) return 180 - phi;	//	reflect across 90 deg symmetry axis
			else return fmod(540 - phi,360);	//	reflect across 270 deg symmetry axis
		}
	}
	//	symmetry across phi = 0, 90, 180, 270 deg axes
	else if (Isym == 4) {
		if (phi >= 0 && phi <= 90) return phi;	//	data points
		else if ((phi > 90 && phi <= 180)) return 180 - phi;	//	reflect across 90 deg symmetry axis
		else if ((phi > 180 && phi < 270)) return phi - 180;	//	rotate thru 180 deg ( = TWO reflections!)
		else return 360 - phi;	//	reflect across 0 deg symmetry axis
	}
	else return phi;
}


void	Tregenza::summary()
{
	//	Tregenza angles summary
	cout << "NTrgz: " << NTrgz() << "\n";
//	for (int iTrgz=0; iTrgz< NTrgz(); iTrgz++) {
//		cout << iTrgz << " " << iTheta(iTrgz) << " " << jPhi(iTrgz) << " " << Theta(iTrgz) <<  " " << Phi(iTrgz) << "\n";
//	}
}

int	Tregenza::nearestc(Double arcdistmax, BGL::vector3 dirext, vector<struct nearestdata>& nd)
{
	vector<Double>	distmin;
	vector<int>		distminindx;
	double			arcdist0;

	int ii, jj;
	for (ii=0; ii<NTrgz(); ii++) {	//	UGH! searches entire list of Tregenza directions
		arcdist0 = arcdist(dirext,dir(ii));
		if (arcdist0 < arcdistmax) {
			distminindx.push_back(ii);
			distmin.push_back(arcdist0);
		}
	}

	//	clean up: sort index list by arcdist and remove duplicates
	//	sort into target arrays
	vector<int> tdminindx;
	vector<Double> tdmin;
	for (ii=0; ii<(int)distminindx.size(); ii++) {
		for (jj=0; jj<(int)tdminindx.size(); jj++) {
			if (distminindx[ii] == tdminindx[jj]) goto IIEND;	//	skip duplicates
			if (distmin[ii] < tdmin[jj]) break;
		}
		//	do insert
		tdminindx.insert(tdminindx.begin()+jj,distminindx[ii]);
		tdmin.insert(tdmin.begin()+jj,distmin[ii]);
		IIEND:;
	}
	//	"finished" result of searches
//	cout << "nearestc: \n";
//	for (int kk=0; kk<tdminindx.size(); kk++) {
//		cout << tdminindx[kk] << " ";
//	}
//	cout << "\n";
//	return tdminindx;
	nd.resize(tdminindx.size());
	for (ii=0; ii<(int)tdminindx.size(); ii++) {
		nd[ii].indx = tdminindx[ii];
		nd[ii].adist = tdmin[ii];
	}
	return tdminindx.size();
}

