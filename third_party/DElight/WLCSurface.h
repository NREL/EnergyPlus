// WLCSurface.h
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
class WLCSurface : public BGL::surf3  
{
public:
	/* -------------------- constructors -------------------- */
	WLCSurface();
	WLCSurface(string InName, BGL::point3 origin, Double Sazm, Double Stilt, Double Zeta, vector<BGL::point2> p2List, Double maxNodeArea);
	WLCSurface(string InName, BGL::point3 origin, Double Sazm, Double Stilt, Double Zeta, vector<BGL::point2> p2List, Double maxNodeArea, Double visrefl);
	WLCSurface(string InName, BGL::point3 origin, Double Sazm, Double Stilt, Double Zeta, Double width, Double height, Double maxNodeArea);
	WLCSurface(string InName, BGL::point3 origin, Double Sazm, Double Stilt, Double Zeta, Double width, Double height, Double maxNodeArea, Double visrefl);

	WLCSurface(string InName, vector<BGL::point3> p3List, Double maxNodeArea);
	// standalone helper for previous form
	void WLCSurfInit(string InName, vector<BGL::point3> p3List, Double maxNodeArea);

	~WLCSurface();


	// -------------------- methods -------------------- 
//	see surf3, plane3 methods
	Double		Area() const;
	Double		VisRefl();
	void		Dump();

	Int			MeshGrid1(Double nodeArea_max);
	Int			MeshGrid2(Double nodeArea_max);
	Int			MeshCutout(const BGL::poly2&);
	Int			MeshSize();
	Double		TotMeshArea() {return mesh.TotArea();}
	BGL::point2	NodePosition2D(Int NodeIterator);
	BGL::point3	NodePosition3D(Int NodeIterator);
	Double		NodeArea(Int NodeIterator);	
	Double		NodeTotIllum(Int NodeIterator);
	Double		NodeTotLum(Int NodeIterator);
	Double		NodeOmega(Int NodeIterator, BGL::point3 extPoint);	
	Int			NearestNodeIndx(BGL::point2 extPoint);

	void		SetNodeData(Int NodeIterator, NodeData data);
	NodeData	GetNodeData(Int NodeIterator);
	void		NodeDump();

	BGL::vector3	DirNodetoExt(Int NodeIterator, BGL::point3 ExtPoint);

private:
//	see also surf3, plane3 members

	Double		vis_refl;
	NodeMesh2	mesh;

};

void SurfNodeIllumPlotArray( vector<WLCSurface *> pWLCSurfaceList, string outfilename);