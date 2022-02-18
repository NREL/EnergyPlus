#ifndef _structures_h
#define _structures_h

// for FILE

#include <stdlib.h>
#include <vector>
#include <set>
#include <limits>
#include <mgl2/define.h>

/*
	for use in s_hull_pro.cpp

	S-hull-pro, Copyright (c) 2012
	Dr David SInclair
	Cambridge, UK

	email david@s-hull.org
*/

// Global replace int->long by A.Balakin 21 April 2014 -- 64bit version can handle huge data arrays

struct Triad
{
	long a,b, c;
	long ab, bc, ac;  // adjacent edges index to neighbouring triangle.
	double ro, R,C;
	//std::set<long> idx;
	Triad() {a=b=c=0;	ab=bc=ac=-1;	ro=-1;	R=C=0;}	// added by A.Balakin 21 April 2014 -- uninitialised variable
	Triad(long x, long y) : a(x), b(y),c(0), ab(-1), bc(-1), ac(-1), ro(-1), R(0), C(0) {}
	Triad(long x, long y, long z) : a(x), b(y), c(z),  ab(-1), bc(-1), ac(-1), ro(-1), R(0), C(0) {}
	Triad(const Triad &p) : a(p.a), b(p.b), c(p.c), ab(p.ab), bc(p.bc), ac(p.ac), ro(p.ro), R(p.R), C(p.C) {}

	Triad &operator=(const Triad &p)
	{
		a = p.a;
		b = p.b;
		c = p.c;

		ab = p.ab;
		bc = p.bc;
		ac = p.ac;

		ro = p.ro;
		R = p.R;
		C = p.C;

		return *this;
	}
};



/* point structure for s_hull only.
   has to keep track of triangle ids as hull evolves.
*/


struct Shx
{
	long id, trid;
	double r,c, tr,tc, ro;
	Shx() {r=c=tr=tc=ro=0;	id=-1;	trid=0;}	// added by A.Balakin 21 April 2014 -- uninitialised variable
	Shx(double a, double b) : r(a), c(b)
	{	trid=0;	ro=tr=tc=0;	id=-1;	}			// added by A.Balakin 21 April 2014 -- uninitialised variable
	Shx(double a, double b, double x) : r(a), c(b), ro(x)
	{	trid=0;	tr=tc=0;	id=-1;	}			// added by A.Balakin 21 April 2014 -- uninitialised variable
	Shx(const Shx &p) : id(p.id), trid(p.trid), r(p.r), c(p.c), tr(p.tr), tc(p.tc), ro(p.ro) {}

	Shx &operator=(const Shx &p)
	{
		id = p.id;
		trid = p.trid;
		r = p.r;
		c = p.c;
		tr = p.tr;
		tc = p.tc;
		ro = p.ro;
		return *this;
	}
};


// sort into descending order (for use in corner responce ranking).
inline bool operator<(const Shx &a, const Shx &b)
{
	if( a.ro == b.ro) {
		if( a.r == b.r ) {
			return a.c < b.c;
		}
		return a.r < b.r;
	}
	return a.ro <  b.ro;
};


struct Dupex
{
	long id;
	double r,c;

	Dupex() {	r=c=0;	id=-1;	}	// added by A.Balakin 21 April 2014 -- uninitialised variable
	Dupex(double a, double b) : id(-1), r(a), c(b) {}
	Dupex(double a, double b, long x) : id(x), r(a), c(b) {}
	Dupex(const Dupex &p) : id(p.id),  r(p.r), c(p.c) {}

	Dupex &operator=(const Dupex &p)
	{
		id = p.id;
		r = p.r;
		c = p.c;
		return *this;
	}
};



// sort into descending order (for use in corner responce ranking).
inline bool operator<(const Dupex &a, const Dupex &b)
{
	if( a.r == b.r)
		return a.c < b.c;
	return a.r <  b.r;
};





// from s_hull.C
long s_hull_pro( std::vector<Shx> &pts, std::vector<Triad> &triads);
void circle_cent2(double r1,double c1, double r2,double c2, double r3,double c3,double &r,double &c, double &ro2);
void circle_cent4(double r1,double c1, double r2,double c2, double r3,double c3,double &r,double &c, double &ro2);
void write_Shx(std::vector<Shx> &pts, char * fname);
void write_Triads(std::vector<Triad> &ts, char * fname);
long Cline_Renka_test(double &Ax, double &Ay, double &Bx, double &By, double &Cx, double &Cy, double &Dx, double &Dy);
long T_flip_pro( std::vector<Shx> &pts, std::vector<Triad> &triads, std::vector<long> &slump, long numt, long start, std::set<long> &ids);
long T_flip_pro_idx( std::vector<Shx> &pts, std::vector<Triad> &triads, std::vector<long> &slump, std::set<long> &ids);

long read_Shx(std::vector<Shx> &pts, char * fname);
long de_duplicate( std::vector<Shx> &pts,  std::vector<long> &outx, Dupex epsilon = Dupex(std::numeric_limits<float>::epsilon(),std::numeric_limits<float>::epsilon()) );
long de_duplicateX( std::vector<Shx> &pts, std::vector<long> &outx,std::vector<Shx> &pts2 );
long  test_center(Shx &pt0, Shx &pt1,Shx &pt2);

long T_flip_edge( std::vector<Shx> &pts, std::vector<Triad> &triads, std::vector<long> &slump, long numt, long start, std::set<long> &ids);


#endif
