// hemisphiral.cpp
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
#pragma warning(disable:4786)

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm> // for min/max
using namespace std;

// BGLincludes
#include "BGL.h"
//using namespace BldgGeomLib;
namespace BGL = BldgGeomLib;

#include "helpers.h"
#include "hemisphiral.h"

// writewndo() Error handler include
#include "DElightManagerC.h"

#ifndef INFINITY
extern double INFINITY;
#endif
extern double NaN_QUIET;
extern double NaN_SIGNAL;
extern double MAXPointTol;

HemiSphiral::HemiSphiral()
: N(0), zMin(-1), deltaz(0), omega(0), DA(0), pitch(0)
{
//	zMin = -1;	//	zMax always= +1; zMin = -1 -> full sphere
}

HemiSphiral::HemiSphiral(Double z)
: N(0), zMin(z), deltaz(0), omega(0), DA(0), pitch(0)
{
//	zMin = -1;	//	zMax always= +1; zMin = -1 -> full sphere
//	zMin = 0;	//	zMax always= +1; zMin =  0 -> hemisphere
}

void HemiSphiral::init()
{
	if (N <= 0) {
		deltaz =  0;
		omega = 0;
		DA = 0;
		pitch = 0;
	}
	else if (N == 1) {
		deltaz =  2;
		omega = 4*PI;
		DA = sqrt(omega);
		pitch = DA/(2*PI);
	}
	else {
		deltaz =  (1 - zMin)/(Double)(N-1);
	//	omega = (1 - zMin)*2*PI;
		omega = (1 - zMin)*2*PI/((N-1) + (1-zMin)/2);
		DA = sqrt(omega);
		pitch = DA/(2*PI);
	}
}

HemiSphiral::HemiSphiral(int n)
: N(n), zMin(-1)
{
//	zMin = -1;	//	zMax always= +1; zMin = -1 -> full sphere
	if (N <= 0) N = 0;
	valList = vector<Double>(N,0);
	init();
}


HemiSphiral::HemiSphiral(Double z, int n)
: N(n), zMin(z)
{
	if (N <= 0) N = 0;
	valList = vector<Double>(N,0);
	init();
}

HemiSphiral::HemiSphiral(Double z, vector<Double>& data)
: zMin(z)
{
	N = data.size();
	valList = data;
	init();
}

void	HemiSphiral::resize(int nsize)
{
	N = nsize;
}

int	HemiSphiral::size()
{
	return N;
}

Double	HemiSphiral::valMax()
{
	return valList[valMaxIndx()];
}

int	HemiSphiral::valMaxIndx()
{
	Double	vmax = -INFINITY;
	int		vmaxindx = 0;
	for (int ii=0; ii<(int)valList.size(); ii++) {
		if ( valList[ii] > vmax ) {
			vmax = valList[ii];
			vmaxindx = ii;
		}
	}
	return vmaxindx;
}

Double	HemiSphiral::valMin()
{
	return valList[valMinIndx()];
}

int	HemiSphiral::valMinIndx()
{
	Double	vmin = INFINITY;
	int		vminindx = 0;
	for (int ii=0; ii<(int)valList.size(); ii++) {
		if ( valList[ii] < vmin ) {
			vmin = valList[ii];
			vminindx = ii;
		}
	}
	return vminindx;
}

Double	HemiSphiral::costheta(int ii)
{
	if ( (ii<0) || (ii>=N) ) return 1;	//	bounds check

	Double	z = 1 - ii*deltaz;
	if (z <= -1.) return  -1.;
	if (z >= 1.) return  1.;
	return z;
}

Double	HemiSphiral::theta(int ii)
{
	return acos(costheta(ii));
}

Double	HemiSphiral::phi(int ii)
{
	return theta(ii)/pitch;;
}

Double	HemiSphiral::phiMod2pi(int ii)
{
	return fmod(phi(ii),2*PI);
}


Double	HemiSphiral::turnsTot()
{
//	return (1 - zMin)/(4*pitch);
	return SLTot()/4;
}

Double	HemiSphiral::phiTot()
{
	return 2*PI*turnsTot();
}

Double	HemiSphiral::SLTot()
{
	if (N==0) return 0;
	if (N==1) return 2*PI;

	return (1 - zMin)/pitch;	//	black magic!
}

Double	HemiSphiral::SLcum(int ii)
{
	if (ii<0) return 0;	//	bounds check
	if (ii>=N) return SLTot();	//	bounds check

	if (N==0) return 0;
	if (N==1) return SLTot();
	return ii*SLTot()/(N-1);
}

Double	HemiSphiral::x2D(int ii)
{
	return dir(ii)[0];
}

Double	HemiSphiral::y2D(int ii)
{
	return dir(ii)[1];
}

BGL::vector3	HemiSphiral::dir(int ii)
{
	if (ii<0) return BGL::vector3(0,0,1);	//	bounds check
	if (ii>=N) return BGL::vector3(0,0,-1);	//	bounds check
	if (N==0) return BGL::vector3(0,0,0);	//	BAD!!!
	if (N==1) return BGL::vector3(0,0,1);

	Double	z, r, phi;

	z = 1 - ii*deltaz;	//	z = cos(theta(ii))
	r = sqrt(1-z*z);	//	r = sin(theta(ii))
	phi = acos(z)/pitch;	//	phi = theta/pitch

	return BGL::vector3(r*cos(phi),r*sin(phi),z);
}

HemiSphiral&	HemiSphiral::operator += (const HemiSphiral&	hs)
{
	for (int ii=0; ii<(int)valList.size(); ii++) valList[ii] += hs[ii];
    return(SELF);
}

HemiSphiral&	HemiSphiral::operator -= (const HemiSphiral&	hs)
{
	for (int ii=0; ii<(int)valList.size(); ii++) valList[ii] -= hs[ii];
    return(SELF);
}

HemiSphiral&	HemiSphiral::operator *= (Double s)
{
	for (int ii=0; ii<(int)valList.size(); ii++) valList[ii] *= s;
    return(SELF);
}

HemiSphiral&	HemiSphiral::operator /= (Double s)
{
	for (int ii=0; ii<(int)valList.size(); ii++) valList[ii] /= s;
    return(SELF);
}

HemiSphiral		HemiSphiral::operator + (const HemiSphiral&	hs) const
{
    HemiSphiral result(zMin, valList.size());
	for (int ii=0; ii<(int)valList.size(); ii++) result[ii] = valList[ii] + hs[ii];
    return(result);
}

HemiSphiral		HemiSphiral::operator - (const HemiSphiral&	hs) const
{
    HemiSphiral result(zMin, valList.size());
	for (int ii=0; ii<(int)valList.size(); ii++) result[ii] = valList[ii] - hs[ii];
    return(result);
}

HemiSphiral		HemiSphiral::operator - () const                // -hs
{
    HemiSphiral result(zMin, valList.size());
	for (int ii=0; ii<(int)valList.size(); ii++) result[ii] = -valList[ii];
    return(result);
}

HemiSphiral		HemiSphiral::operator * (Double s) const          // hs * s
{
    HemiSphiral result(zMin, valList.size());
	for (int ii=0; ii<(int)valList.size(); ii++) result[ii] = valList[ii]*s;
    return(result);
}

HemiSphiral		HemiSphiral::operator / (Double s) const          // hs / s
{
    HemiSphiral result(zMin, valList.size());
	for (int ii=0; ii<(int)valList.size(); ii++) result[ii] = valList[ii]/s;
    return(result);
}

//	NOTE:  NOT member of HemiSphiral::
Double	arcdist(BGL::vector3 dir1, BGL::vector3 dir2)
{
//	return acos(Max(Min(BGL::dot(dir1,dir2),1.),-1.));
	return acos(max(min(BGL::dot(dir1,dir2),1.),-1.));
}

HemiSphiral		HemiSphiral::reflect(bool b0, bool b1, bool b2) {
    HemiSphiral result(zMin, valList.size());
	BGL::vector3 dir1;
	for (int ii=0; ii<(int)valList.size(); ii++) {
		dir1 = result.dir(ii);
		if (b0) dir1[0] *= -1;	//	do x-reflection
		if (b1) dir1[1] *= -1;	//	do y-reflection
		if (b2) dir1[2] *= -1;	//	do z-reflection
		result.valList[ii] = interp(dir1);
	}
	return result;
}

vector<int>	HemiSphiral::nearest4(Double phiext, Double thetaext)
{

	BGL::vector3	dirext;
	dirext[0] = sin(thetaext)*cos(phiext);
	dirext[1] = sin(thetaext)*sin(phiext);
	dirext[2] = cos(thetaext);

	return nearest4(dirext);
}

vector<int>	HemiSphiral::nearest4(BGL::vector3 dirext)
{
	vector<Double> distmax(4,+INFINITY);
	vector<int> distmaxindx(4);

	Double dist;
	for (int jj=0; jj < size(); jj++) {
	// searches entire dataset - ugh!
		dist = arcdist(dirext,dir(jj));
		if (  dist < distmax[3]) {
			distmax[3] = dist;
			distmaxindx[3] = jj;
		}
		if (  dist < distmax[2]) {
			distmax[3] = distmax[2];
			distmaxindx[3] = distmaxindx[2];
			distmax[2] = dist;
			distmaxindx[2] = jj;
		}
		if (  dist < distmax[1]) {
			distmax[2] = distmax[1];
			distmaxindx[2] = distmaxindx[1];
			distmax[1] = dist;
			distmaxindx[1] = jj;
		}
		if (  dist < distmax[0]) {
			distmax[1] = distmax[0];
			distmaxindx[1] = distmaxindx[0];
			distmax[0] = dist;
			distmaxindx[0] = jj;
		}
	}
	return distmaxindx;

}

vector<int>	HemiSphiral::nearestn(int nnear, BGL::vector3 dirext)
{
	vector<Double> distmin(nnear,+INFINITY);
	vector<int> distminindx(nnear);

	Double dist;
	for (int jj=0; jj < size(); jj++) {
	// searches entire dataset - ugh!
		dist = arcdist(dirext,dir(jj));
		//	do sorted insertion
		for (int kk=nnear-1; kk>=0; kk--) {
			if (  dist < distmin[kk]) {
				if (kk < nnear - 1) {
				//	move old stuff down the array
					distmin[kk+1] = distmin[kk];
					distminindx[kk+1] = distminindx[kk];
				}
				//	then insert new stuff in array at position kk
				distmin[kk] = dist;
				distminindx[kk] = jj;
			}
		}
	}
	return distminindx;

}

int	HemiSphiral::nearestc(Double admax, BGL::vector3 dirext, vector<struct nearestdata>& nd)
{
	vector<Double> distmin;
	vector<int> distminindx;
	if (N <= 0) return 0;	//	zero length

	//	find starting search row and 1st search point on it
//	int		jj0 =      0.5 + (1 - dirext[2])/deltaz;
	int		jj0 =      (int)(0.5 + (1 - dirext[2])/deltaz);
//	Double	jj0Double = (1 - dirext[2])/deltaz;
//	cout << jj0 << " " << 1. - jj0*deltaz << " " << jj0Double << "\n";

	//	search row 0
	int		irow = 0;
//	cout << "irow search: " << irow << "; jjstart: " << jj0 << "\n";
	int		jj0best = rowsearch(jj0,admax,dirext,distminindx,distmin);
	//	use jj0best as start for subsequent row searches

	int		jj;
	Double	ad;
	//	search previous rows
	while (1) {
		irow -= 1;
//		jj = 0.5 + (1 - cos(theta(jj0best) + irow*2*PI*pitch))/deltaz;
		jj = (int) (0.5 + (1 - cos(theta(jj0best) + irow*2*PI*pitch))/deltaz);
		ad = arcdist(dirext,dir(jj));
//		cout << jj << " " << ad << "\n";
		if (ad > admax) break;
//		cout << "irow search: " << irow << "; jjstart: " << jj << "\n";
		rowsearch(jj,admax,dirext,distminindx,distmin);
	}

	irow = 0;
	//	search later rows
	while (1) {
		irow += 1;
//		jj = 0.5 + (1 - cos(theta(jj0best) + irow*2*PI*pitch))/deltaz;
		jj = (int) (0.5 + (1 - cos(theta(jj0best) + irow*2*PI*pitch))/deltaz);
		ad = arcdist(dirext,dir(jj));
//		cout << jj << " " << ad << "\n";
		if (ad > admax) break;
//		cout << "irow search: " << irow << "; jjstart: " << jj << "\n";
		rowsearch(jj,admax,dirext,distminindx,distmin);
	}
	//	"raw" result of searches
//	cout << "nearestc: \n";
//	for (int kk=0; kk<distminindx.size(); kk++) {
//		cout << distminindx[kk] << " ";
//	}
//	cout << "\n";

	//	clean up: sort index list by arcdist and remove duplicates
	//	sort into target arrays
	vector<int> tdminindx;
	vector<Double> tdmin;
	int ii;
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

int HemiSphiral::rowsearch(int jjstart, Double arcdistmax, BGL::vector3 dirext, vector<int>& distmaxindx, vector<Double>& distmax)
{
	Double	distmin = 2*PI;
	int		indxmin = jjstart;
	Double	arcdist0, arcdist1;
	int		searchcount = 0;

	int		jj = jjstart;
	//	evaluate jjstart
	arcdist0 = arcdist(dirext,dir(jj));
	if (arcdist0 < arcdistmax) {
//		cout << "rs: " << jj << " " << arcdist0 << "\n";
		distmaxindx.push_back(jj);
		distmax.push_back(arcdist0);
		if (arcdist0 < distmin) {
			distmin = arcdist0;
			indxmin = jj;
		}
		searchcount += 1;
	}

	//	search up
	jj = jjstart;
	while (jj >= 1 && jj < N-1) {
		jj += 1;
		arcdist1 = arcdist(dirext,dir(jj));
		if (arcdist1 < distmin) {
			distmin = arcdist1;
			indxmin = jj;
		}
		if(arcdist1 < arcdist0) {
			if (arcdist1 > arcdistmax) continue;
		}
		else if (arcdist1 > arcdistmax) break;
		if (arcdist1 < distmin) {
			distmin = arcdist1;
			indxmin = jj;
		}
//		cout << "rs: " << jj << " " << arcdist1 << "\n";
		distmaxindx.push_back(jj);
		distmax.push_back(arcdist1);
		arcdist0 = arcdist1;
		searchcount += 1;
	}
	// then search down
	jj = jjstart;
	while (jj >= 1 && jj < N-1) {
		jj -= 1;
		arcdist1 = arcdist(dirext,dir(jj));
		if(arcdist1 < arcdist0) {
			if (arcdist1 > arcdistmax) continue;
		}
		else if (arcdist1 > arcdistmax) break;
		if (arcdist1 < distmin) {
			distmin = arcdist1;
			indxmin = jj;
		}
//		cout << "rs: " << jj << " " << arcdist1 << "\n";
		distmaxindx.push_back(jj);
		distmax.push_back(arcdist1);
		arcdist0 = arcdist1;
		searchcount += 1;
	}
	//	return nearest (best) point for use in subsequent row searches
	return indxmin;

}

Double	HemiSphiral::interp(Double phiext, Double thetaext)
{
	return interp(AnglesToDir3D(phiext,thetaext));
}


Double	HemiSphiral::interp(BGL::vector3 dirext)
{

	vector<int>		nearestindx;
	vector<struct nearestdata> nd;

	//	1.5*DA will always generate about 8 positions (????),
	//	independent of N
//	nearestindx = nearestc(2.0*DA,dirext);
	int	nnsize = nearestc(2.0*DA,dirext, nd);
	if (nnsize == 0) return 0;

	vector<Double> weights = ::interpwgts(nd);

	Double interpval = 0;
	for (int ii=0; ii<(int)weights.size(); ii++) {
		interpval += weights[ii]*valList[nd[ii].indx];
	}

	return interpval;
}

//vector<Double>	HemiSphiral::interpwgts(BGL::vector3 dirext, vector<struct nearestdata>& nd)
vector<Double>	interpwgts(vector<struct nearestdata>& nd)
{
	//	avoids singularity if dirext == dir(ii)
	Double epsilon = MAXPointTol;	//	tunable parameter

	vector<Double>	weights;	//	zero length
//	int	size = nearestindx.size();
	int	size = nd.size();
	if (size == 0) return weights;
	if (size == 1) {
		weights.push_back(1);
		return weights;
	}
	if (size > 4) size = 4;	//	max of 4 nearest points used for interpolation
	weights.resize(size,0);
	vector<Double> adist(size,0);
	vector<Double> adistinverse(size,0);
	Double	sum = 0;
	int ii;
	for (ii=0; ii<size; ii++) {
		adist[ii] = max(epsilon,nd[ii].adist);
		adistinverse[ii] = 1/adist[ii];
		sum += adistinverse[ii];
	}
	for (ii=0; ii<size; ii++) {
		weights[ii] = adistinverse[ii] / sum;
	}

	return weights;
}

vector<Double>	HemiSphiral::interpwgts(BGL::vector3 dirext)
{
	vector<struct nearestdata> nd;
	nearestc(1.5*DA,dirext, nd);

//	return interpwgts(dirext,nearestc(1.5*DA,dirext));
	return ::interpwgts(nd);
}

Double		HemiSphiral::TotIllum()
{
	Double	sum = 0;
	for (int ii=0; ii<N; ii++) sum += valList[ii];
	return sum*omega;
}

Double		HemiSphiral::TotPlanarIllum(BGL::vector3 dirplane)
{
	Double	sum = 0;
	Double	Dot;
	for (int ii=0; ii<N; ii++) {
		Dot = BGL::dot(dir(ii),dirplane);
		if (Dot > 0) sum += valList[ii]*Dot;
	}
	return sum*omega;
}

Double		HemiSphiral::TotHorizIllum()
{
	return TotPlanarIllum();
}

void	HemiSphiral::plotarray(vector<vector<Double> >& PlotArray, BGL::RHCoordSys3 LCS)
{
	Double	x2D, y2D;
	BGL::vector3	dirLCS, dirWCS;
	Double	val;

	int		vsize = PlotArray.size();
	for (int irow=0; irow<vsize; irow++) {
		y2D = 2*(0.5 - (Double)irow/vsize - 1./(2.*vsize));
		for (int icol=0; icol<vsize; icol++) {
			x2D = 2*((Double)icol/vsize - 0.5 + 1./(2.*vsize));

			if (BGL::dist(BGL::point2(x2D,y2D),BGL::point2(0,0)) > 1) {
				PlotArray[irow][icol] = NaN_SIGNAL;
			}
			else {
				//	calc dirLCS on sphere by mapping from 2D point on plot view-plane
				dirLCS[0] = x2D;
				dirLCS[1] = y2D;
				dirLCS[2] = sqrt(1 - x2D*x2D - y2D*y2D);

				//	transform from dirLCS of plot view-plane to dirWCS of the sphere
				//	XXX: this dircos transform needs to be put into RHCoordsys3 object!!!
				dirWCS[0] = dirLCS[0]*LCS[0][0] + dirLCS[1]*LCS[1][0] + dirLCS[2]*LCS[2][0];
				dirWCS[1] = dirLCS[0]*LCS[0][1] + dirLCS[1]*LCS[1][1] + dirLCS[2]*LCS[2][1];
				dirWCS[2] = dirLCS[0]*LCS[0][2] + dirLCS[1]*LCS[1][2] + dirLCS[2]*LCS [2][2];
				val = interp(dirWCS);
				PlotArray[irow][icol] = val;
			}
		}
	}
}

ofstream&	HemiSphiral::plotfile(ofstream& outfile, int vsize, Double phi, Double theta, Double zeta)
{
//	outfile << "view-plane: phi: " << phi << ";  theta:" << theta << "; zeta: " << zeta << "\n";
//	int	vsize = 40;
	vector<vector<Double> > PlotArray;
	PlotArray.resize(vsize);
	int irow;
	for (irow=0; irow<vsize; irow++) PlotArray[irow].resize(vsize);

	BGL::RHCoordSys3 LCS(phi,theta,zeta);

	plotarray(PlotArray,LCS);

	for (irow=0; irow<vsize; irow++) {
		for (int icol=0; icol<vsize; icol++) {
			outfile << PlotArray[irow][icol] << ' ';
		}
		outfile << '\n';
	}
	outfile << '\n';
	return outfile;
}

void	HemiSphiral::plotview(int vsize, Double vMin, Double vMax, Double phi, Double theta, Double zeta)
{
//	cout << "view-plane: phi: " << RadToDeg(phi) << ";  theta:" << RadToDeg(theta) << "; zeta: " << RadToDeg(zeta) << "\n";
//	int	vsize = 40;
	vector<vector<Double> > PlotArray;
	PlotArray.resize(vsize);
	int irow;
	for (irow=0; irow<vsize; irow++) PlotArray[irow].resize(vsize);

	BGL::RHCoordSys3 LCS(phi,theta,zeta);

	plotarray(PlotArray,LCS);

	double	vmax0 = valMax();
	double	vmin0 = valMin();
	double	vmax;
	double	vmin;
	if (vMin == -99.) vmin = vmin0;
	else vmin = vMin;
	if (vMax == -99.) vmax = vmax0;
	else vmax = vMax;
	cout << "dataMax: " << valMax() << " dataMin: " << valMin() << " plotMax: " << vmax << " plotMin: " << vmin << "\n";

	double SymbolRatio;
	for (irow=0; irow<vsize; irow++) {
		for (int icol=0; icol<vsize; icol++) {
			if (vmax0 == vmin0) cout << '=';
			else if (PlotArray[irow][icol] == NaN_SIGNAL) cout << '.';
			else if (PlotArray[irow][icol] < vmin) cout << '-';
			else {
				SymbolRatio = (PlotArray[irow][icol] - vmin)/(vmax - vmin);
				if (SymbolRatio > 1) cout << '+';
				else {
//					cout << (char) ('0' + 1 + 8*((PlotArray[irow][icol] - vmin)/(vmax - vmin)));
					cout << (char) ('0' + 10*(SymbolRatio));
				}
			}
//			cout << ' ';
		}
		cout << '\n';
	}
	cout << '\n';
}

ifstream&	HemiSphiral::load(ifstream& infile)
//	reads one HemiSphiral dataset from a filestream that have more than one dataset
{
    HemiSphiral    result;
    Char    c;
	std::ostringstream osstream;

	// Expected format: {double0 double1 ... doubleN}

    while (infile >> c && isspace(c)) {;} // skip through spaces
    if (infile.eof()) return(infile);
	if (infile.fail()) {
		osstream << "HemiSphiral:ReadError1: unrecoverable failbit\n";
	    writewndo(osstream.str(),"e");
		return(infile);
	}

    if (c != '{') {
		infile.putback(c);
	    infile.clear(ios::failbit);
		osstream << "HemiSphiral:ReadError2: Expected '{' - got \'" << c << "\'" << "\n";
	    writewndo(osstream.str(),"e");
	    return(infile);
	}
	//	else ...
	Double data;
	vector<Double> dataList;
	while (infile >> data) dataList.push_back(data);
	infile.clear();

	while (infile >> c && isspace(c));
	if (c != '}')	{
		infile.putback(c);
    	infile.clear(ios::failbit);
	    osstream << "HemiSphiral:ReadError3: Expected '}' - got \'" << c << "\'" << "\n";
	    writewndo(osstream.str(),"e");
	    return(infile);
    }

	if (dataList.size() == 0) {
    	infile.clear(ios::failbit);
	    osstream << "HemiSphiral:ReadError4: dataList empty" << "\n";
	    writewndo(osstream.str(),"e");
	    return(infile);
	}

	result.valList = dataList;
	result.N = dataList.size();
	result.init();
	*this = result;

    return(infile);



}

ofstream& HemiSphiral::save(ofstream& outfile)
{
	outfile << "{\n";	//	delim
	for(int ii=0; ii<size(); ii++) {
		outfile << valList[ii] << "\n";
	}
	outfile << "}\n";	//	delim
	return outfile;
}

void HemiSphiral::summary()
{
	cout << "size: " << size() << "\n";
//	cout << "sizeof(): " << sizeof(*this) + sizeof(this->valList) << "\n";
	cout << "omega: " << omega << " = " << omega/PI << "*PI\n";
	cout << "DA: " << DA << "\n";
	cout << "Spiral pitch: " << pitch << "\n";
	if (size()) {
		cout << "turnsTot: " << turnsTot() << "\n";
		cout << "phiTot: " << phiTot() << " rad = " << phiTot()*180/PI << " deg\n";
		cout << "SpiralArcLengthTot: " << SLTot() << "; deltaArcLen: " << SLTot()/(N-1) << "\n";
		cout << "SphArea: " << DA*SLTot()/PI << "*PI" << "\n";
		cout << "Ncalc: " << SLTot()/DA << "\n";
	}
}

void	HemiSphiral::pointdump()
{
	for (int ii=0; ii<N; ii++) {
		cout << right << setw(4) << ii << " ";
		cout << left << setprecision(5);
		cout << setw(8) << valList[ii] << " ";
		cout << setw(8) << theta(ii)*180/PI << " ";
		cout << setw(8) << phiMod2pi(ii)*180/PI << " ";
		cout << setw(8) << phi(ii)/(2*PI) << " ";	//	= #turns
		cout << setw(8) << SLcum(ii) << " ";
		cout << setw(8) << dir(ii) << " ";
		cout << "\n";
	}
}

ofstream&	HemiSphiral::pointdumpT21(ofstream& outfile)
{
	int	ii = -1;
	while (dir(++ii)[2] >= 0) {
//	for (int ii=0; ii<N; ii++) {
//		outfile << right << setw(4) << ii << "\t";
		outfile << left << setprecision(6);
		Double	phiT21 = 180 - phiMod2pi(ii)*180/PI;
		if (phiT21 < 0) phiT21 += 360;
		outfile << setw(8) << phiT21 << "\t";
		Double	thetaT21 = 180 - theta(ii)*180/PI;
		outfile << setw(8) << thetaT21 << "\t";
		outfile << setw(8) << valList[ii] << "\t";
//		outfile << setw(8) << dir(ii) << "\t";
		outfile << "\n";
	}
	return outfile;
}



