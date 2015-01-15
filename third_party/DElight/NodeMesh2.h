//	NodeMesh2.h
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
struct	NodeData
{
				NodeData() : CFSIllum(0), WindowIllum(0), NodeIllum(0) { }
	Double		CFSIllum;
	Double		WindowIllum;
	Double		NodeIllum;
};

struct	Node
{
	Double		area;
	BGL::point2	position;
	NodeData	data;

	Double		Area(){return area;};
};

struct	NodeMesh2
{
				NodeMesh2() { }
				NodeMesh2(const BGL::poly2& p2, Double maxNodeArea) {	grid1(p2, maxNodeArea);}
    Node        &operator [] (Int i);
    const Node  &operator [] (Int i) const;

	Int			size() {return meshList.size();};
	Double		TotArea();
	void		SummaryDump();
	void		NodeDump();

	Int			NearestToPext(BGL::point2 PExt);

	Int			grid1(const BGL::poly2&, int);	//	min required nodes
	Int			grid1(const BGL::poly2&, Double);	//	max allowed node area
	Int			grid2(const BGL::poly2&, Double);	//	adaptive method - not used

	Int			remove(const BGL::poly2&);

	vector<Node>	meshList;
};

inline Node &NodeMesh2::operator [] (Int i)
{
    return(meshList[i]);
}

inline const Node &NodeMesh2::operator [] (Int i) const
{
    return(meshList[i]);
}
