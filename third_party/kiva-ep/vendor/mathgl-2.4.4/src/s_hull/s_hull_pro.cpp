#include <iostream>
//#include <hash_set.h>
//#include <hash_set>
#include <set>
#include <vector>
#include <fstream>
#include <stdlib.h>
#include <math.h>
#include <string>
#include <algorithm>


#include "s_hull_pro.h"

using namespace std;


/* copyright 2012 Dr David Sinclair
   david@s-hull.org

	program to compute Delaunay triangulation of a set of points.

	this code may not be published or distributed without the concent of the copyright holder.

 */


// Global replace int->long by A.Balakin 21 April 2014 -- 64bit version can handle huge data arrays


void circle_cent2(double r1,double c1, double r2,double c2, double r3,double c3,
				  double &r,double &c, double &ro2)
{
	/*
	 *  function to return the center of a circle and its radius
	 * degenerate case should never be passed to this routine!!!!!!!!!!!!!
	 * but will return r0 = -1 if it is.
	 */

	double a1 = (r1+r2)/2.0;
	double a2 = (c1+c2)/2.0;
	double b1 = (r3+r2)/2.0;
	double b2 = (c3+c2)/2.0;

	double e2 = r1-r2;
	double e1 = -c1+c2;

	double q2 = r3-r2;
	double q1 = -c3+c2;

	r=0;
	c=0;
	ro2=-1;
	if( e1*-q2 + e2*q1 == 0 ) return;

	double beta = (-e2*(b1-a1) + e1*(b2-a2))/( e2*q1-e1*q2);

	r = b1 + q1*beta;
	c = b2 + q2*beta;

	ro2 = (r1-r)*(r1-r) + (c1-c)*(c1-c);
	return;
}


/*
    read an ascii file of (r,c) point pairs.

    the first line of the points file should contain
    "NUMP  2 points"

    if it does not have the word points in it the first line is
    interpretted as a point pair.

 */

long read_Shx(std::vector<Shx> &pts, char * fname)
{
	char s0[513];
	long nump =0;
	double p1,p2;

	Shx pt;

	std::string line;
	std::string points_str("points");

	std::ifstream myfile;
	myfile.open(fname);

	if (myfile.is_open())
	{

		getline (myfile,line);
		//long numc = line.length();

		// check string for the string "points"
		long n = (long) line.find( points_str);
		if( n > 0)
		{
			while ( myfile.good() )
			{
				getline (myfile,line);
				if( line.length() <= 512)
				{
					copy( line.begin(), line.end(), s0);
					s0[line.length()] = 0;
					long v = sscanf( s0, "%lg %lg", &p1,&p2);
					if( v>0 ) {
						pt.id = nump;
						nump++;
						pt.r = p1;
						pt.c = p2;
						pts.push_back(pt);
					}
				}
			}
		}
		else    // assume all number pairs on a line are points
		{
			if( line.length() <= 512)
			{
				copy( line.begin(), line.end(), s0);
				s0[line.length()] = 0;
				long v = sscanf( s0, "%lg %lg", &p1,&p2);
				if( v>0 )
				{
					pt.id = nump;
					nump++;
					pt.r = p1;
					pt.c = p2;
					pts.push_back(pt);
				}
			}

			while ( myfile.good() )
			{
				getline (myfile,line);
				if( line.length() <= 512)
				{
					copy( line.begin(), line.end(), s0);
					s0[line.length()] = 0;
					long v = sscanf( s0, "%lg %lg", &p1,&p2);
					if( v>0 )
					{
						pt.id = nump;
						nump++;
						pt.r = p1;
						pt.c = p2;
						pts.push_back(pt);
					}
				}
			}
		}
		myfile.close();
	}

	nump = (long) pts.size();

	return(nump);
}

/*
	write out a set of points to disk


*/

void write_Shx(std::vector<Shx> &pts, char * fname)
{
	std::ofstream out(fname, ios::out);

	long nr = (long) pts.size();
	out << nr << " 2 points" << endl;

	for (long r = 0; r < nr; r++)
	{
		out << pts[r].r << ' ' << pts[r].c <<  endl;
	}
	out.close();

	return;
}



/*
 write out triangle ids to be compatible with matlab/octave array numbering.

 */
void write_Triads(std::vector<Triad> &ts, char * fname)
{
	std::ofstream out(fname, ios::out);

	long nr = (long) ts.size();
	out << nr << " 6   point-ids (1,2,3)  adjacent triangle-ids ( limbs ab  ac  bc )" << endl;

	for (long r = 0; r < nr; r++)
	{
		out << ts[r].a+1 << ' ' << ts[r].b+1 <<' ' << ts[r].c+1 <<' '
			<< ts[r].ab+1 <<' ' << ts[r].ac+1 <<' ' << ts[r].bc+1 << endl; //" " << ts[r].ro <<  endl;
	}
	out.close();

	return;
}





/*  version in which the ids of the triangles associated with the sides of the hull are tracked.


 */

long s_hull_pro( std::vector<Shx> &pts, std::vector<Triad> &triads)
{

	long nump = (long) pts.size();

	if( nump < 3 )
	{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//		cerr << "less than 3 points, aborting " << endl;
		return(-1);
	}


	double r = pts[0].r;
	double c = pts[0].c;
	for( long k=0; k<nump; k++)
	{
		double dr = pts[k].r-r;
		double dc = pts[k].c-c;

		pts[k].ro = dr*dr + dc*dc;

	}

	sort( pts.begin(), pts.end() );


	double r1 = pts[0].r;
	double c1 = pts[0].c;

	double r2 = pts[1].r;
	double c2 = pts[1].c;
	long mid = -1;
	double romin2 =  9.0e20, ro2, R=0,C=0;	// added by A.Balakin 21 April 2014 -- uninitialised variable

	long k=2;
	while (k<nump)
	{

		circle_cent2(r1,c1,r2,c2,  pts[k].r,  pts[k].c, r,c,ro2);
		if( ro2 < romin2 && ro2 > 0 )
		{
			mid = k;
			romin2 = ro2;
			R = r;
			C = c;

		}
		else if( romin2 *4 < pts[k].ro )
			k=nump;

		k++;
	}

	if( mid < 0 )
	{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//		cerr << "linear structure, aborting " << endl;
		return(-2);
	}


	Shx pt0 = pts[0];
	Shx pt1 = pts[1];
	Shx pt2 = pts[mid];

	pts.erase(pts.begin() + mid);  // necessary for round off reasons:((((((
	pts.erase(pts.begin() );
	pts.erase(pts.begin() );

	for( long k=0; k<nump-3; k++)
	{
		double dr = pts[k].r-R;
		double dc = pts[k].c-C;

		pts[k].ro = dr*dr + dc*dc;

	}

	sort( pts.begin(), pts.end() );

	pts.insert(pts.begin(), pt2);
	pts.insert(pts.begin(), pt1);
	pts.insert(pts.begin(), pt0);

	std::vector<long> slump;
	slump.resize(nump);

	for( long k=0; k<nump; k++)
	{
		if( pts[k].id < nump)
		{
			slump[ pts[k].id] = k;
		}
		else
		{
			long mx = pts[k].id+1;
			while( (long) slump.size() <= mx)
			{
				slump.push_back(0);
			}
			slump[pts[k].id] = k;
		}
	}

	std::vector<Shx> hull;


	r = (pts[0].r + pts[1].r + pts[2].r )/(double) 3.0;
	c = (pts[0].c + pts[1].c + pts[2].c )/(double) 3.0;

	double dr0 = pts[0].r - r,  dc0 = pts[0].c - c;
	double tr01 =  pts[1].r - pts[0].r, tc01 =  pts[1].c - pts[0].c;

	double df = -tr01* dc0 + tc01*dr0;
	if( df < 0 )    // [ 0 1 2 ]
	{
		pt0.tr = pt1.r-pt0.r;
		pt0.tc = pt1.c-pt0.c;
		pt0.trid = 0;
		hull.push_back( pt0 );

		pt1.tr = pt2.r-pt1.r;
		pt1.tc = pt2.c-pt1.c;
		pt1.trid = 0;
		hull.push_back( pt1 );

		pt2.tr = pt0.r-pt2.r;
		pt2.tc = pt0.c-pt2.c;
		pt2.trid = 0;
		hull.push_back( pt2 );


		Triad tri(pt0.id,pt1.id,pt2.id);
		tri.ro = romin2;
		tri.R = R;
		tri.C = C;

		triads.push_back(tri);

	}
	else           // [ 0 2 1 ] as anti-clockwise turning is the work of the devil....
	{
		pt0.tr = pt2.r-pt0.r;
		pt0.tc = pt2.c-pt0.c;
		pt0.trid = 0;
		hull.push_back( pt0 );

		pt2.tr = pt1.r-pt2.r;
		pt2.tc = pt1.c-pt2.c;
		pt2.trid = 0;
		hull.push_back( pt2 );

		pt1.tr = pt0.r-pt1.r;
		pt1.tc = pt0.c-pt1.c;
		pt1.trid = 0;
		hull.push_back( pt1 );

		Triad tri(pt0.id,pt2.id,pt1.id);
		tri.ro = romin2;
		tri.R = R;
		tri.C = C;
		triads.push_back(tri);
	}

	// add new points into hull (removing obscured ones from the chain)
	// and creating triangles....
	// that will need to be flipped.

	double dr, dc, rx,cx;
	Shx  ptx;
	long numt=0;	// added by A.Balakin 21 April 2014 -- uninitialised variable

	for( long k=3; k<nump; k++)
	{
		rx = pts[k].r;
		cx = pts[k].c;
		ptx.r = rx;
		ptx.c = cx;
		ptx.id = pts[k].id;

		long numh = (long) hull.size()/*, numh_old = numh*/;	// commented by A.Balakin 21 April 2014 -- unused variable
		dr = rx- hull[0].r;
		dc = cx- hull[0].c;  // outwards pointing from hull[0] to pt.

		std::vector<long> pidx, tridx;
		long hidx;  // new hull point location within hull.....


		double df = -dc* hull[0].tr + dr*hull[0].tc;    // visibility test vector.
		if( df < 0 )   // starting with a visible hull facet !!!
		{
//			long e1 = 1, e2 = numh;	// commented by A.Balakin 21 April 2014 -- unused variable
			hidx = 0;

			// check to see if segment numh is also visible
			df = -dc* hull[numh-1].tr + dr*hull[numh-1].tc;
			//cerr << df << ' ' ;
			if( df < 0 )     // visible.
			{
				pidx.push_back(hull[numh-1].id);
				tridx.push_back(hull[numh-1].trid);


				for( long h=0; h<numh-1; h++)
				{
					// if segment h is visible delete h
					dr = rx- hull[h].r;
					dc = cx- hull[h].c;
					df = -dc* hull[h].tr + dr*hull[h].tc;
					pidx.push_back(hull[h].id);
					tridx.push_back(hull[h].trid);
					if( df < 0 )
					{
						hull.erase(hull.begin() + h);
						h--;
						numh--;
					}
					else 	  // quit on invisibility
					{
						ptx.tr = hull[h].r - ptx.r;
						ptx.tc = hull[h].c - ptx.c;

						hull.insert( hull.begin() , ptx);
						numh++;
						break;
					}
				}
				// look backwards through the hull structure.

				for( long h=numh-2; h>0; h--)
				{
					// if segment h is visible delete h + 1
					dr = rx- hull[h].r;
					dc = cx- hull[h].c;
					df = -dc* hull[h].tr + dr*hull[h].tc;

					if( df < 0 )   // h is visible
					{
						pidx.insert(pidx.begin(), hull[h].id);
						tridx.insert(tridx.begin(), hull[h].trid);
						hull.erase(hull.begin() + h+1);  // erase end of chain

					}
					else
					{

						h = (long) hull.size()-1;
						hull[h].tr = -hull[h].r + ptx.r;   // points at start of chain.
						hull[h].tc = -hull[h].c + ptx.c;
						break;
					}
				}

				df = 9;

			}
			else
			{
				//	cerr << df << ' ' << endl;
				hidx = 1;  // keep pt hull[0]
				tridx.push_back(hull[0].trid);
				pidx.push_back(hull[0].id);

				for( long h=1; h<numh; h++)
				{
					// if segment h is visible delete h
					dr = rx- hull[h].r;
					dc = cx- hull[h].c;
					df = -dc* hull[h].tr + dr*hull[h].tc;
					pidx.push_back(hull[h].id);
					tridx.push_back(hull[h].trid);
					if( df < 0 )                      // visible
					{
						hull.erase(hull.begin() + h);
						h--;
						numh--;
					}
					else 	  // quit on invisibility
					{
						ptx.tr = hull[h].r - ptx.r;
						ptx.tc = hull[h].c - ptx.c;

						hull[h-1].tr = ptx.r - hull[h-1].r;
						hull[h-1].tc = ptx.c - hull[h-1].c;

						hull.insert( hull.begin()+h, ptx);
						break;
					}
				}
			}

			df = 8;

		}
		else
		{
			long e1 = -1,  e2 = numh;
			for( long h=1; h<numh; h++)
			{
				dr = rx- hull[h].r;
				dc = cx- hull[h].c;
				df = -dc* hull[h].tr + dr*hull[h].tc;
				if( df < 0 )
				{
					if( e1 < 0 ) e1 = h;  // fist visible
				}
				else
				{
					if( e1 > 0 )  // first invisible segment.
					{
						e2 = h;
						break;
					}
				}

			}

	if (e1 < 0) {
		// Cannot find visible point - it might be caused by numerical issues on some kind of datasets
		return (-5);
	}

			// triangle pidx starts at e1 and ends at e2 (inclusive).
			if( e2 < numh )
			{
				for( long e=e1; e<=e2; e++)
				{
					pidx.push_back(hull[e].id);
					tridx.push_back(hull[e].trid);
				}
			}
			else
			{
				for( long e=e1; e<e2; e++)
				{
					pidx.push_back(hull[e].id);
					tridx.push_back(hull[e].trid);   // there are only n-1 triangles from n hull pts.
				}
				pidx.push_back(hull[0].id);
			}


			// erase elements e1+1 : e2-1 inclusive.

			if( e1 < e2-1)
			{
				hull.erase(hull.begin() + e1+1, hull.begin()+ e2);
			}

			// insert ptx at location e1+1.
			if( e2 == numh)
			{
				ptx.tr = hull[0].r - ptx.r;
				ptx.tc = hull[0].c - ptx.c;
			}
			else
			{
				ptx.tr = hull[e1+1].r - ptx.r;
				ptx.tc = hull[e1+1].c - ptx.c;
			}

			hull[e1].tr = ptx.r - hull[e1].r;
			hull[e1].tc = ptx.c - hull[e1].c;

			hull.insert( hull.begin()+e1+1, ptx);
			hidx = e1+1;

		}


		long a = ptx.id, T0;
		Triad trx( a, 0,0);
		r1 = pts[slump[a]].r;
		c1 = pts[slump[a]].c;

		long npx = (long) pidx.size()-1;
		numt = (long) triads.size();
		T0 = numt;

		if( npx == 1)
		{
			trx.b = pidx[0];
			trx.c = pidx[1];

			trx.bc = tridx[0];
			trx.ab = -1;
			trx.ac = -1;

			// index back into the triads.
			Triad &txx = triads[tridx[0]];
			if( ( trx.b == txx.a && trx.c == txx.b) |( trx.b == txx.b && trx.c == txx.a))
			{
				txx.ab = numt;
			}
			else if( ( trx.b == txx.a && trx.c == txx.c) |( trx.b == txx.c && trx.c == txx.a))
			{
				txx.ac = numt;
			}
			else if( ( trx.b == txx.b && trx.c == txx.c) |( trx.b == txx.c && trx.c == txx.b))
			{
				txx.bc = numt;
			}


			hull[hidx].trid = numt;
			if( hidx > 0 )
				hull[hidx-1].trid = numt;
			else
			{
				numh = (long) hull.size();
				hull[numh-1].trid = numt;
			}
			triads.push_back( trx );
			numt++;
		}

		else
		{
			trx.ab = -1;
			for(long p=0; p<npx; p++)
			{
				trx.b = pidx[p];
				trx.c = pidx[p+1];


				trx.bc = tridx[p];
				if( p > 0 )
					trx.ab = numt-1;
				trx.ac = numt+1;

				// index back into the triads.
				Triad &txx = triads[tridx[p]];
				if( ( trx.b == txx.a && trx.c == txx.b) |( trx.b == txx.b && trx.c == txx.a))
				{
					txx.ab = numt;
				}
				else if( ( trx.b == txx.a && trx.c == txx.c) |( trx.b == txx.c && trx.c == txx.a))
				{
					txx.ac = numt;
				}
				else if( ( trx.b == txx.b && trx.c == txx.c) |( trx.b == txx.c && trx.c == txx.b))
				{
					txx.bc = numt;
				}

				triads.push_back( trx );
				numt++;
			}
			triads[numt-1].ac=-1;

			hull[hidx].trid = numt-1;
			if( hidx > 0 )
				hull[hidx-1].trid = T0;
			else
			{
				numh = (long) hull.size();
				hull[numh-1].trid = T0;
			}


		}

	}

// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//	cerr << "of triangles " << triads.size() << " to be flipped. "<< endl;

	//  write_Triads(triads, "tris0.mat");

	std::set<long> ids;

	long tf = T_flip_pro( pts, triads, slump, numt, 0, ids);
	if( tf < 0 )
	{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//		cerr << "cannot triangualte this set " << endl;
		return(-3);
	}

	//  write_Triads(triads, "tris1.mat");

	long nits = (long) ids.size(), nit=1;
	while(  nits > 0 && nit < 50)
	{

		tf = T_flip_pro_idx( pts, triads, slump, ids);
		nits = (long) ids.size();
		nit ++;
		if( tf < 0 )
		{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//			cerr << "cannot triangualte this set " << endl;
			return(-4);
		}
	}

	ids.clear();
	nits = T_flip_edge( pts, triads, slump, numt, 0, ids);
	nit=0;

	while(  nits > 0 && nit < 100) {

		tf = T_flip_pro_idx( pts, triads, slump, ids);
		nits = (long) ids.size();
		//	cerr << "flipping cycle  " << nit << "   active triangles " << nits << endl;
		nit ++;
		if( tf < 0 )
		{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//			cerr << "cannot triangualte this set " << endl;
			return(-4);
		}
	}
	return(1);
}


void circle_cent4(double r1,double c1, double r2,double c2, double r3,double c3,
				  double &r,double &c, double &ro2)
{
	/*
	 *  function to return the center of a circle and its radius
	 * degenerate case should never be passed to this routine!!!!!!!!!!!!!
	 * but will return r0 = -1 if it is.
	 */

	double rd, cd;
	double v1 = 2*(r2-r1), v2 = 2*(c2-c1), v3 = r2*r2 - r1*r1 + c2*c2 - c1*c1;
	double v4 = 2*(r3-r1),
		   v5 = 2*(c3-c1),
		   v6 = r3*r3 - r1*r1 + c3*c3 - c1*c1,

		   v7 =  v2*v4 - v1*v5;
	if( v7 == 0 )
	{
		r=0;
		c=0;
		ro2 = -1;
		return;
	}

	cd = (v4*v3 - v1*v6)/v7;
	if( v1 != 0 )
		rd = (v3 - c*v2)/v1;
	else
		rd = (v6 - c*v5)/v4;

	ro2 = (double)  ( (rd-r1)*(rd-r1) + (cd-c1)*(cd-c1) );
	r = (double) rd;
	c = (double) cd;

	return;
}


namespace {

/**
 * Rounds the value given to the nearest multiple of the epsilon given
 */
double coarsen(const double value, const double epsilon) {
	const double minimal_epsilon = std::numeric_limits<double>::epsilon() * value;
	return (epsilon < minimal_epsilon) ? value : floor(value / epsilon + 0.5) * epsilon;
}

}

/* test a set of points for duplicates.

   erase duplicate points, do not change point ids.

*/

long de_duplicate( std::vector<Shx> &pts, std::vector<long> &outx, const Dupex epsilon ) {
	long nump = (long) pts.size();
	std::vector<Dupex> dpx;
	Dupex d;
	for( long k=0; k<nump; k++) {
		d.r = coarsen(pts[k].r, epsilon.r);
		d.c = coarsen(pts[k].c, epsilon.c);
		d.id = k;
		dpx.push_back(d);
	}

	sort(dpx.begin(), dpx.end());

	for( long k=0; k<nump-1; k++) {
		if( dpx[k].r == dpx[k+1].r && dpx[k].c == dpx[k+1].c )
		{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//			cerr << "duplicate-point ids " << dpx[k].id << "  " << dpx[k+1].id << "   at  ("  << pts[dpx[k+1].id].r << "," << pts[dpx[k+1].id].c << ")" << endl;
			outx.push_back( dpx[k+1].id);
		}
	}

	if( outx.size() == 0 )
		return(0);

	sort(outx.begin(), outx.end());

	long nx = (long) outx.size();
	for( long k=nx-1; k>=0; k--) {
		pts.erase(pts.begin()+outx[k]);
	}

	return(nx);
}




/*
   flip pairs of triangles that are not valid delaunay triangles
   the Cline-Renka test is used rather than the less stable circum
   circle center computation test of s-hull.

   or the more expensive determinant test.

 */


long T_flip_pro( std::vector<Shx> &pts, std::vector<Triad> &triads, std::vector<long> &slump, long numt, long start, std::set<long> &ids)
{

	double r3,c3;
	long pa,pb,pc, pd, D, L1, L2, L3, L4, T2;

	Triad tx, tx2;


	for( long t=start; t<numt; t++)
	{

		Triad &tri = triads[t];
		// test all 3 neighbours of tri

		long flipped = 0;

		if( tri.bc >= 0 )
		{

			pa = slump[tri.a];
			pb = slump[tri.b];
			pc = slump[tri.c];

			T2 = tri.bc;
			Triad &t2 = triads[T2];
			// find relative orientation (shared limb).
			if( t2.ab == t )
			{
				D = t2.c;
				pd = slump[t2.c];

				if( tri.b == t2.a)
				{
					L3 = t2.ac;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ac;
				}
			}
			else if(  t2.ac == t )
			{
				D = t2.b;
				pd = slump[t2.b];

				if( tri.b == t2.a)
				{
					L3 = t2.ab;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ab;
				}
			}
			else if(  t2.bc == t )
			{
				D = t2.a;
				pd = slump[t2.a];

				if( tri.b == t2.b)
				{
					L3 = t2.ab;
					L4 = t2.ac;
				}
				else
				{
					L3 = t2.ac;
					L4 = t2.ab;
				}
			}
			else
			{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//				cerr << "triangle flipping error. " << t << endl;
				return(-5);
			}


// Commented by A.Balakin 21 April 2014 -- unused variable
//			if( pd < 0 || pd > 100)
//				long dfx = 9;

			r3 = pts[pd].r;
			c3 = pts[pd].c;

			long XX = Cline_Renka_test( pts[pa].r, pts[pa].c, pts[pb].r, pts[pb].c,
									   pts[pc].r, pts[pc].c, r3, c3 );

			if( XX < 0 )
			{

				L1 = tri.ab;
				L2 = tri.ac;
				if( L1 != L3 && L2 != L4 )   // need this check for stability.
				{

					tx.a = tri.a;
					tx.b = tri.b;
					tx.c = D;

					tx.ab = L1;
					tx.ac = T2;
					tx.bc = L3;


					// triangle 2;
					tx2.a = tri.a;
					tx2.b = tri.c;
					tx2.c = D;

					tx2.ab = L2;
					tx2.ac = t;
					tx2.bc = L4;


					ids.insert(t);
					ids.insert(T2);

					t2 = tx2;
					tri = tx;
					flipped = 1;

					// change knock on triangle labels.
					if( L3 >= 0 )
					{
						Triad &t3 = triads[L3];
						if( t3.ab == T2 ) t3.ab = t;
						else if( t3.bc == T2 ) t3.bc = t;
						else if( t3.ac == T2 ) t3.ac = t;
					}

					if(L2 >= 0 )
					{
						Triad &t4 = triads[L2];
						if( t4.ab == t ) t4.ab = T2;
						else if( t4.bc == t ) t4.bc = T2;
						else if( t4.ac == t ) t4.ac = T2;
					}
				}
			}
		}


		if(  flipped == 0 && tri.ab >= 0 )
		{

			pc = slump[tri.c];
			pb = slump[tri.b];
			pa = slump[tri.a];

			T2 = tri.ab;
			Triad &t2 = triads[T2];
			// find relative orientation (shared limb).
			if( t2.ab == t )
			{
				D = t2.c;
				pd = slump[t2.c];

				if( tri.a == t2.a)
				{
					L3 = t2.ac;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ac;
				}
			}
			else if(  t2.ac == t )
			{
				D = t2.b;
				pd = slump[t2.b];

				if( tri.a == t2.a)
				{
					L3 = t2.ab;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ab;
				}
			}
			else if(  t2.bc == t )
			{
				D = t2.a;
				pd = slump[t2.a];

				if( tri.a == t2.b)
				{
					L3 = t2.ab;
					L4 = t2.ac;
				}
				else
				{
					L3 = t2.ac;
					L4 = t2.ab;
				}
			}
			else
			{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//				cerr << "triangle flipping error. " << t << endl;
				return(-5);
			}

			r3 = pts[pd].r;
			c3 = pts[pd].c;

			long XX = Cline_Renka_test( pts[pc].r, pts[pc].c, pts[pb].r, pts[pb].c,
									   pts[pa].r, pts[pa].c,r3, c3);

			if( XX < 0)
			{


				L1 = tri.ac;
				L2 = tri.bc;
				if( L1 != L3 && L2 != L4 )   // need this check for stability.
				{

					tx.a = tri.c;
					tx.b = tri.a;
					tx.c = D;

					tx.ab = L1;
					tx.ac = T2;
					tx.bc = L3;


					// triangle 2;
					tx2.a = tri.c;
					tx2.b = tri.b;
					tx2.c = D;

					tx2.ab = L2;
					tx2.ac = t;
					tx2.bc = L4;


					ids.insert(t);
					ids.insert(T2);

					t2 = tx2;
					tri = tx;
					flipped = 1;

					// change knock on triangle labels.
					if( L3 >= 0 )
					{
						Triad &t3 = triads[L3];
						if( t3.ab == T2 ) t3.ab = t;
						else if( t3.bc == T2 ) t3.bc = t;
						else if( t3.ac == T2 ) t3.ac = t;
					}

					if(L2 >= 0 )
					{
						Triad &t4 = triads[L2];
						if( t4.ab == t ) t4.ab = T2;
						else if( t4.bc == t ) t4.bc = T2;
						else if( t4.ac == t ) t4.ac = T2;
					}

				}

			}
		}


		if( flipped == 0 && tri.ac >= 0 )
		{

			pc = slump[tri.c];
			pb = slump[tri.b];
			pa = slump[tri.a];

			T2 = tri.ac;
			Triad &t2 = triads[T2];
			// find relative orientation (shared limb).
			if( t2.ab == t )
			{
				D = t2.c;
				pd = slump[t2.c];

				if( tri.a == t2.a)
				{
					L3 = t2.ac;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ac;
				}
			}
			else if(  t2.ac == t )
			{
				D = t2.b;
				pd = slump[t2.b];

				if( tri.a == t2.a)
				{
					L3 = t2.ab;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ab;
				}
			}
			else if(  t2.bc == t )
			{
				D = t2.a;
				pd = slump[t2.a];

				if( tri.a == t2.b)
				{
					L3 = t2.ab;
					L4 = t2.ac;
				}
				else
				{
					L3 = t2.ac;
					L4 = t2.ab;
				}
			}
			else
			{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//				cerr << "triangle flipping error. " << t << endl;
				return(-5);
			}

			r3 = pts[pd].r;
			c3 = pts[pd].c;

			long XX = Cline_Renka_test( pts[pb].r, pts[pb].c, pts[pa].r, pts[pa].c,
									   pts[pc].r, pts[pc].c,r3, c3);

			if( XX < 0 )
			{

				L1 = tri.ab;   // .ac shared limb
				L2 = tri.bc;
				if( L1 != L3 && L2 != L4 )   // need this check for stability.
				{

					tx.a = tri.b;
					tx.b = tri.a;
					tx.c = D;

					tx.ab = L1;
					tx.ac = T2;
					tx.bc = L3;


					// triangle 2;
					tx2.a = tri.b;
					tx2.b = tri.c;
					tx2.c = D;

					tx2.ab = L2;
					tx2.ac = t;
					tx2.bc = L4;

					ids.insert(t);
					ids.insert(T2);

					t2 = tx2;
					tri = tx;

					// change knock on triangle labels.
					if( L3 >= 0 )
					{
						Triad &t3 = triads[L3];
						if( t3.ab == T2 ) t3.ab = t;
						else if( t3.bc == T2 ) t3.bc = t;
						else if( t3.ac == T2 ) t3.ac = t;
					}

					if(L2 >= 0 )
					{
						Triad &t4 = triads[L2];
						if( t4.ab == t ) t4.ab = T2;
						else if( t4.bc == t ) t4.bc = T2;
						else if( t4.ac == t ) t4.ac = T2;
					}

				}
			}
		}


	}


	return(1);
}

/* minimum angle cnatraint for circum circle test.
   due to Cline & Renka

   A   --	B

   |	/	|

   C   --	D


 */

long Cline_Renka_test(double &Ax, double &Ay,
					 double &Bx, double &By,
					 double &Cx, double &Cy,
					 double &Dx, double &Dy)
{

	double v1x = Bx-Ax, v1y = By-Ay,	v2x = Cx-Ax, v2y = Cy-Ay,
		  v3x = Bx-Dx, v3y = By-Dy,	v4x = Cx-Dx, v4y = Cy-Dy;
	double cosA = v1x*v2x + v1y*v2y;
	double cosD = v3x*v4x + v3y*v4y;

	if( cosA < 0 && cosD < 0 ) // two obtuse angles
		return(-1);

//	double ADX = Ax-Dx, ADy = Ay-Dy;	// commented by A.Balakin 21 April 2014 -- unused variable


	if( cosA > 0 && cosD > 0 )  // two acute angles
		return(1);


	double sinA = fabs(v1x*v2y - v1y*v2x);
	double sinD = fabs(v3x*v4y - v3y*v4x);

	if( cosA*sinD + sinA*cosD < 0 )
		return(-1);

	return(1);

}




// same again but with set of triangle ids to be iterated over.


long T_flip_pro_idx( std::vector<Shx> &pts, std::vector<Triad> &triads, std::vector<long> &slump, std::set<long> &ids)
{

	double  r3,c3;
	long pa,pb,pc, pd,  D, L1, L2, L3, L4, T2;

	Triad tx, tx2;
	std::set<long> ids2;
	ids2.clear();

	std::set<long> :: const_iterator x=ids.begin();
	while(x != ids.end() )
	{
		long t = *x;
		x++;


		Triad &tri = triads[t];
		// test all 3 neighbours of tri
		long flipped = 0;



		if( tri.bc >= 0 )
		{

			pa = slump[tri.a];
			pb = slump[tri.b];
			pc = slump[tri.c];

			T2 = tri.bc;
			Triad &t2 = triads[T2];
			// find relative orientation (shared limb).
			if( t2.ab == t )
			{
				D = t2.c;
				pd = slump[t2.c];

				if( tri.b == t2.a)
				{
					L3 = t2.ac;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ac;
				}
			}
			else if(  t2.ac == t )
			{
				D = t2.b;
				pd = slump[t2.b];

				if( tri.b == t2.a)
				{
					L3 = t2.ab;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ab;
				}
			}
			else if(  t2.bc == t )
			{
				D = t2.a;
				pd = slump[t2.a];

				if( tri.b == t2.b)
				{
					L3 = t2.ab;
					L4 = t2.ac;
				}
				else
				{
					L3 = t2.ac;
					L4 = t2.ab;
				}
			}
			else
			{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//				cerr << "triangle flipping error. " << t << "  T2: " <<  T2<<  endl;
				return(-6);
			}

			r3 = pts[pd].r;
			c3 = pts[pd].c;

			long XX = Cline_Renka_test( pts[pa].r, pts[pa].c, pts[pb].r, pts[pb].c,
									   pts[pc].r, pts[pc].c,r3, c3);

			if( XX < 0 )
			{
				L1 = tri.ab;
				L2 = tri.ac;

				if( L1 != L3 && L2 != L4 )   // need this check for stability.
				{


					tx.a = tri.a;
					tx.b = tri.b;
					tx.c = D;

					tx.ab = L1;
					tx.ac = T2;
					tx.bc = L3;


					// triangle 2;
					tx2.a = tri.a;
					tx2.b = tri.c;
					tx2.c = D;

					tx2.ab = L2;
					tx2.ac = t;
					tx2.bc = L4;

					ids2.insert(t);
					ids2.insert(T2);

					t2 = tx2;
					tri = tx;
					flipped = 1;

					// change knock on triangle labels.
					if( L3 >= 0 )
					{
						Triad &t3 = triads[L3];
						if( t3.ab == T2 ) t3.ab = t;
						else if( t3.bc == T2 ) t3.bc = t;
						else if( t3.ac == T2 ) t3.ac = t;
					}

					if(L2 >= 0 )
					{
						Triad &t4 = triads[L2];
						if( t4.ab == t ) t4.ab = T2;
						else if( t4.bc == t ) t4.bc = T2;
						else if( t4.ac == t ) t4.ac = T2;
					}

				}
			}
		}


		if( flipped == 0 && tri.ab >= 0 )
		{

			pc = slump[tri.c];
			pb = slump[tri.b];
			pa = slump[tri.a];

			T2 = tri.ab;
			Triad &t2 = triads[T2];
			// find relative orientation (shared limb).
			if( t2.ab == t )
			{
				D = t2.c;
				pd = slump[t2.c];

				if( tri.a == t2.a)
				{
					L3 = t2.ac;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ac;
				}
			}
			else if(  t2.ac == t )
			{
				D = t2.b;
				pd = slump[t2.b];

				if( tri.a == t2.a)
				{
					L3 = t2.ab;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ab;
				}
			}
			else if(  t2.bc == t )
			{
				D = t2.a;
				pd = slump[t2.a];

				if( tri.a == t2.b)
				{
					L3 = t2.ab;
					L4 = t2.ac;
				}
				else
				{
					L3 = t2.ac;
					L4 = t2.ab;
				}
			}
			else
			{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//				cerr << "triangle flipping error. " << t <<  endl;
				return(-6);
			}

			r3 = pts[pd].r;
			c3 = pts[pd].c;

			long XX = Cline_Renka_test( pts[pc].r, pts[pc].c, pts[pb].r, pts[pb].c,
									   pts[pa].r, pts[pa].c,r3, c3);

			if( XX < 0 )
			{
				L1 = tri.ac;
				L2 = tri.bc;
				if( L1 != L3 && L2 != L4 )   // need this check for stability.
				{

					tx.a = tri.c;
					tx.b = tri.a;
					tx.c = D;

					tx.ab = L1;
					tx.ac = T2;
					tx.bc = L3;


					// triangle 2;
					tx2.a = tri.c;
					tx2.b = tri.b;
					tx2.c = D;

					tx2.ab = L2;
					tx2.ac = t;
					tx2.bc = L4;


					ids2.insert(t);
					ids2.insert(T2);

					t2 = tx2;
					tri = tx;
					flipped = 1;

					// change knock on triangle labels.
					if( L3 >= 0 )
					{
						Triad &t3 = triads[L3];
						if( t3.ab == T2 ) t3.ab = t;
						else if( t3.bc == T2 ) t3.bc = t;
						else if( t3.ac == T2 ) t3.ac = t;
					}

					if(L2 >= 0 )
					{
						Triad &t4 = triads[L2];
						if( t4.ab == t ) t4.ab = T2;
						else if( t4.bc == t ) t4.bc = T2;
						else if( t4.ac == t ) t4.ac = T2;
					}

				}
			}
		}


		if( flipped == 0 && tri.ac >= 0 )
		{

			pc = slump[tri.c];
			pb = slump[tri.b];
			pa = slump[tri.a];

			T2 = tri.ac;
			Triad &t2 = triads[T2];
			// find relative orientation (shared limb).
			if( t2.ab == t )
			{
				D = t2.c;
				pd = slump[t2.c];

				if( tri.a == t2.a)
				{
					L3 = t2.ac;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ac;
				}
			}
			else if(  t2.ac == t )
			{
				D = t2.b;
				pd = slump[t2.b];

				if( tri.a == t2.a)
				{
					L3 = t2.ab;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ab;
				}
			}
			else if(  t2.bc == t )
			{
				D = t2.a;
				pd = slump[t2.a];

				if( tri.a == t2.b)
				{
					L3 = t2.ab;
					L4 = t2.ac;
				}
				else
				{
					L3 = t2.ac;
					L4 = t2.ab;
				}
			}
			else
			{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//				cerr << "triangle flipping error. " << t << endl;
				return(-6);
			}

			r3 = pts[pd].r;
			c3 = pts[pd].c;

			long XX = Cline_Renka_test( pts[pb].r, pts[pb].c, pts[pc].r, pts[pc].c,
									   pts[pa].r, pts[pa].c,r3, c3);

			if( XX < 0 )
			{
				L1 = tri.ab;   // .ac shared limb
				L2 = tri.bc;
				if( L1 != L3 && L2 != L4 )   // need this check for stability.
				{
					tx.a = tri.b;
					tx.b = tri.a;
					tx.c = D;

					tx.ab = L1;
					tx.ac = T2;
					tx.bc = L3;

					// triangle 2;
					tx2.a = tri.b;
					tx2.b = tri.c;
					tx2.c = D;

					tx2.ab = L2;
					tx2.ac = t;
					tx2.bc = L4;

					ids2.insert(t);
					ids2.insert(T2);

					t2 = tx2;
					tri = tx;

					// change knock on triangle labels.
					if( L3 >= 0 )
					{
						Triad &t3 = triads[L3];
						if( t3.ab == T2 ) t3.ab = t;
						else if( t3.bc == T2 ) t3.bc = t;
						else if( t3.ac == T2 ) t3.ac = t;
					}

					if(L2 >= 0 )
					{
						Triad &t4 = triads[L2];
						if( t4.ab == t ) t4.ab = T2;
						else if( t4.bc == t ) t4.bc = T2;
						else if( t4.ac == t ) t4.ac = T2;
					}


				}
			}
		}
	}

	ids.clear();
	ids.insert(ids2.begin(), ids2.end());

	return(1);
}

/* test the seed configuration to see if the center
   of the circum circle lies inside the seed triangle.

   if not issue a warning.
*/


long  test_center(Shx &pt0, Shx &pt1,Shx &pt2)
{
	double r01 = pt1.r - pt0.r;
	double c01 = pt1.c - pt0.c;

	double r02 = pt2.r - pt0.r;
	double c02 = pt2.c - pt0.c;

	double r21 = pt1.r - pt2.r;
	double c21 = pt1.c - pt2.c;

	double v = r01*r02 + c01*c02;
	if( v < 0 ) return(-1);

	v = r21*r02 + c21*c02;
	if( v > 0 ) return(-1);

	v = r01*r21 + c01*c21;
	if( v < 0 ) return(-1);

	return(1);
}

long de_duplicateX( std::vector<Shx> &pts, std::vector<long> &outx,std::vector<Shx> &pts2 )
{
	long nump = (long) pts.size();
	std::vector<Dupex> dpx;
	Dupex d;
	for( long k=0; k<nump; k++)
	{
		d.r = pts[k].r;
		d.c = pts[k].c;
		d.id = k;
		dpx.push_back(d);
	}

	sort(dpx.begin(), dpx.end());

// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//	cerr << "de-duplicating ";
	pts2.clear();
	pts2.push_back(pts[dpx[0].id]);
	pts2[0].id = 0;
	long cnt = 1;

	for( long k=0; k<nump-1; k++)
	{
		if( dpx[k].r == dpx[k+1].r && dpx[k].c == dpx[k+1].c )
		{
			//cerr << "duplicate-point ids " << dpx[k].id << "  " << dpx[k+1].id << "   at  ("  << pts[dpx[k+1].id].r << "," << pts[dpx[k+1].id].c << ")" << endl;
			//cerr << dpx[k+1].id << " ";

			outx.push_back( dpx[k+1].id);
		}
		else
		{
			pts[dpx[k+1].id].id = cnt;
			pts2.push_back(pts[dpx[k+1].id]);
			cnt++;
		}
	}

// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//	cerr << "removed  " << outx.size() << endl;

	return(outx.size());
}



long T_flip_edge( std::vector<Shx> &pts, std::vector<Triad> &triads, std::vector<long> &slump, long numt, long start, std::set<long> &ids)
{
	double r3,c3;
	long pa,pb,pc, pd, D, L1, L2, L3, L4, T2;

	Triad tx, tx2;


	for( long t=start; t<numt; t++)
	{
		Triad &tri = triads[t];
		// test all 3 neighbours of tri

		long flipped = 0;

		if( tri.bc >= 0  && (tri.ac < 0 || tri.ab < 0) )
		{
			pa = slump[tri.a];
			pb = slump[tri.b];
			pc = slump[tri.c];

			T2 = tri.bc;
			Triad &t2 = triads[T2];
			// find relative orientation (shared limb).
			if( t2.ab == t )
			{
				D = t2.c;
				pd = slump[t2.c];

				if( tri.b == t2.a)
				{
					L3 = t2.ac;
					L4 = t2.bc;
				}
				else {
					L3 = t2.bc;
					L4 = t2.ac;
				}
			}
			else if(  t2.ac == t )
			{
				D = t2.b;
				pd = slump[t2.b];

				if( tri.b == t2.a)
				{
					L3 = t2.ab;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ab;
				}
			}
			else if(  t2.bc == t )
			{
				D = t2.a;
				pd = slump[t2.a];

				if( tri.b == t2.b)
				{
					L3 = t2.ab;
					L4 = t2.ac;
				}
				else
				{
					L3 = t2.ac;
					L4 = t2.ab;
				}
			}
			else
			{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//				cerr << "triangle flipping error. " << t << endl;
				return(-5);
			}


// Commented by A.Balakin 21 April 2014 -- unused variable
//			if( pd < 0 || pd > 100)
//				long dfx = 9;

			r3 = pts[pd].r;
			c3 = pts[pd].c;

			long XX = Cline_Renka_test( pts[pa].r, pts[pa].c, pts[pb].r, pts[pb].c,
									   pts[pc].r, pts[pc].c, r3, c3 );

			if( XX < 0 ) {

				L1 = tri.ab;
				L2 = tri.ac;
				//	if( L1 != L3 && L2 != L4 ){  // need this check for stability.

				tx.a = tri.a;
				tx.b = tri.b;
				tx.c = D;

				tx.ab = L1;
				tx.ac = T2;
				tx.bc = L3;


				// triangle 2;
				tx2.a = tri.a;
				tx2.b = tri.c;
				tx2.c = D;

				tx2.ab = L2;
				tx2.ac = t;
				tx2.bc = L4;


				ids.insert(t);
				ids.insert(T2);

				t2 = tx2;
				tri = tx;
				flipped = 1;

				// change knock on triangle labels.
				if( L3 >= 0 )
				{
					Triad &t3 = triads[L3];
					if( t3.ab == T2 ) t3.ab = t;
					else if( t3.bc == T2 ) t3.bc = t;
					else if( t3.ac == T2 ) t3.ac = t;
				}

				if(L2 >= 0 )
				{
					Triad &t4 = triads[L2];
					if( t4.ab == t ) t4.ab = T2;
					else if( t4.bc == t ) t4.bc = T2;
					else if( t4.ac == t ) t4.ac = T2;
				}
				//	}
			}
		}


		if(  flipped == 0 && tri.ab >= 0  && (tri.ac < 0 || tri.bc < 0))
		{
			pc = slump[tri.c];
			pb = slump[tri.b];
			pa = slump[tri.a];

			T2 = tri.ab;
			Triad &t2 = triads[T2];
			// find relative orientation (shared limb).
			if( t2.ab == t )
			{
				D = t2.c;
				pd = slump[t2.c];

				if( tri.a == t2.a)
				{
					L3 = t2.ac;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ac;
				}
			}
			else if(  t2.ac == t )
			{
				D = t2.b;
				pd = slump[t2.b];

				if( tri.a == t2.a)
				{
					L3 = t2.ab;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ab;
				}
			}
			else if(  t2.bc == t )
			{
				D = t2.a;
				pd = slump[t2.a];

				if( tri.a == t2.b)
				{
					L3 = t2.ab;
					L4 = t2.ac;
				}
				else
				{
					L3 = t2.ac;
					L4 = t2.ab;
				}
			}
			else {
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//				cerr << "triangle flipping error. " << t << endl;
				return(-5);
			}

			r3 = pts[pd].r;
			c3 = pts[pd].c;

			long XX = Cline_Renka_test( pts[pc].r, pts[pc].c, pts[pb].r, pts[pb].c,
									   pts[pa].r, pts[pa].c,r3, c3);

			if( XX < 0)
			{
				L1 = tri.ac;
				L2 = tri.bc;
				//	if( L1 != L3 && L2 != L4 ){  // need this check for stability.

				tx.a = tri.c;
				tx.b = tri.a;
				tx.c = D;

				tx.ab = L1;
				tx.ac = T2;
				tx.bc = L3;

				// triangle 2;
				tx2.a = tri.c;
				tx2.b = tri.b;
				tx2.c = D;

				tx2.ab = L2;
				tx2.ac = t;
				tx2.bc = L4;

				ids.insert(t);
				ids.insert(T2);

				t2 = tx2;
				tri = tx;
				flipped = 1;

				// change knock on triangle labels.
				if( L3 >= 0 )
				{
					Triad &t3 = triads[L3];
					if( t3.ab == T2 ) t3.ab = t;
					else if( t3.bc == T2 ) t3.bc = t;
					else if( t3.ac == T2 ) t3.ac = t;
				}

				if(L2 >= 0 )
				{
					Triad &t4 = triads[L2];
					if( t4.ab == t ) t4.ab = T2;
					else if( t4.bc == t ) t4.bc = T2;
					else if( t4.ac == t ) t4.ac = T2;
				}
			}
		}


		if( flipped == 0 && tri.ac >= 0  && (tri.bc < 0 || tri.ab < 0) )
		{
			pc = slump[tri.c];
			pb = slump[tri.b];
			pa = slump[tri.a];

			T2 = tri.ac;
			Triad &t2 = triads[T2];
			// find relative orientation (shared limb).
			if( t2.ab == t )
			{
				D = t2.c;
				pd = slump[t2.c];

				if( tri.a == t2.a)
				{
					L3 = t2.ac;
					L4 = t2.bc;
				}
				else
				{
					L3 = t2.bc;
					L4 = t2.ac;
				}
			}
			else if(  t2.ac == t )
			{
				D = t2.b;
				pd = slump[t2.b];

				if( tri.a == t2.a)
				{
					L3 = t2.ab;
					L4 = t2.bc;
				}
				else {
					L3 = t2.bc;
					L4 = t2.ab;
				}
			}
			else if(  t2.bc == t )
			{
				D = t2.a;
				pd = slump[t2.a];

				if( tri.a == t2.b)
				{
					L3 = t2.ab;
					L4 = t2.ac;
				}
				else
				{
					L3 = t2.ac;
					L4 = t2.ab;
				}
			}
			else
			{
// Commented by A.Balakin 21 April 2014 -- library shouldn't print anything
//				cerr << "triangle flipping error. " << t << endl;
				return(-5);
			}

			r3 = pts[pd].r;
			c3 = pts[pd].c;

			long XX = Cline_Renka_test( pts[pb].r, pts[pb].c, pts[pa].r, pts[pa].c,
									   pts[pc].r, pts[pc].c,r3, c3);

			if( XX < 0 )
			{
				L1 = tri.ab;   // .ac shared limb
				L2 = tri.bc;
				//	if( L1 != L3 && L2 != L4 ){  // need this check for stability.

				tx.a = tri.b;
				tx.b = tri.a;
				tx.c = D;

				tx.ab = L1;
				tx.ac = T2;
				tx.bc = L3;


				// triangle 2;
				tx2.a = tri.b;
				tx2.b = tri.c;
				tx2.c = D;

				tx2.ab = L2;
				tx2.ac = t;
				tx2.bc = L4;

				ids.insert(t);
				ids.insert(T2);

				t2 = tx2;
				tri = tx;

				// change knock on triangle labels.
				if( L3 >= 0 )
				{
					Triad &t3 = triads[L3];
					if( t3.ab == T2 ) t3.ab = t;
					else if( t3.bc == T2 ) t3.bc = t;
					else if( t3.ac == T2 ) t3.ac = t;
				}

				if(L2 >= 0 )
				{
					Triad &t4 = triads[L2];
					if( t4.ab == t ) t4.ab = T2;
					else if( t4.bc == t ) t4.bc = T2;
					else if( t4.ac == t ) t4.ac = T2;
				}
			}
		}
	}
	return(1);
}

