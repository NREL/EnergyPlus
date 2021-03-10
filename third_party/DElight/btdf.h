//	btdf.h
// =================================================================================
// Copyright 1992-2009	Regents of University of California
//						Lawrence Berkeley National Laboratory

//  Authors: W.L. Carroll and R.J. Hitchcock
//           Building Technologies Department
//           Lawrence Berkeley National Laboratory

// This work was supported by the Assistant Secretary for Energy Efficiency
// and Renewable Energy, Office of Building Technologies,
// Building Systems and Materials Division of the
// U.S. Department of Energy under Contract No. DE-AC03-76SF00098.

// NOTICE: The Government is granted for itself and others acting on its behalf
// a paid-up, nonexclusive, irrevocable worldwide license in this data to reproduce,
// prepare derivative works, and perform publicly and display publicly.
// Beginning five (5) years after (date permission to assert copyright was obtained),
// subject to two possible five year renewals, the Government is granted for itself
// and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
// license in this data to reproduce, prepare derivative works, distribute copies to
// the public, perform publicly and display publicly, and to permit others to do so.
// NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
// THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL
// LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY
// INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE
// WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
// =================================================================================

//	base class
struct btdf
{
	string	btdftype;
//	HemiSphiral	HSin;	NOT USED in base class
	vector<HemiSphiral> HSoutList;

	btdf();
	btdf(int, int);

	string	type() {return btdftype;}

    virtual	HemiSphiral&        operator [] (int ii) = 0;
	virtual	int			size() = 0 ;

	virtual	int	iiFindDataIndx(int iiTrgz) = 0;
	virtual BGL::vector3	inDir(int ii) = 0;
	virtual Double	inDirOmega(int ii) = 0;
	BGL::vector3	outDir(int ii);
	virtual	int isym() = 0;
	virtual	int iisym(int ii) = 0;
	virtual	int iidata(int ii) = 0;
	virtual Double	qinterp(BGL::vector3, BGL::vector3) = 0;
	virtual Double	qexact(int iin, int jout) = 0;
	virtual	void	summary();
	void	plotview(int first, int last, int viewsize, Double theta =0, Double phi =0, Double zeta =0);
	ifstream&	load(ifstream&);
	virtual	ofstream&	save(ofstream&);
};

//	external btdfLoad function
	btdf*	btdfLoad(ifstream&);

//	specialized derived classes based on input direction scheme

//	HemiSphiral input dirs
struct btdfHS : public btdf
{
	HemiSphiral	HSin;

	btdfHS();
	btdfHS(int, int);
    HemiSphiral&        operator [] (int ii);

	int		size() {return HSoutList.size();}
	int		iiFindDataIndx(int iiTrgz) {return iiTrgz;}
	BGL::vector3	inDir(int ii);
	Double	inDirOmega(int ii) {(void)ii; return HSin.omega;}
	int		isym() {return 0;}
	int		iisym(int ii) {return ii;}
	int		iidata(int ii) {return ii;}
	Double	qinterp(BGL::vector3, BGL::vector3);
	Double	qexact(int iin, int jout);

	void	summary();
	ofstream&	save(ofstream&);
};

//Tregenza input dirs
struct Tregenza
{
	//	NOTE: Phi, Theta are in Degrees
	int			NTheta;
	double		deltaTheta;
	vector<int>	MPhi;
	vector<double>	omega;

	Tregenza();
	~Tregenza();

	int		iTheta(double Theta) {return (int)(Theta/deltaTheta);}
	int		jPhi(double Theta, double Phi) {return (int) (Phi*MPhi[iTheta(Theta)]/360.);}
	int		ii0(int itheta) {if (itheta==0) return 0; else return MPhi[itheta-1] + ii0(itheta-1);}
	int		NTrgz() {return MPhi[NTheta-1] + ii0(NTheta-1);}
	int		iiTrgz(double Theta, double Phi) {return ii0(iTheta(Theta)) + jPhi(Theta,Phi);}
	int		iTheta(int ii) {int itheta; for (itheta=0; itheta<NTheta; itheta++) if (ii < ii0(itheta)) break; return itheta - 1;}
	int		jPhi(int ii) {return ii - ii0(iTheta(ii));}
	double	Theta(int ii) {return deltaTheta*iTheta(ii);}
	double	Phi(int ii) {return jPhi(ii)*360./MPhi[iTheta(ii)];}
	double	PhiSym(double phi, int Isym);
	int		iiSym(int ii, int Isym) {return iiTrgz(Theta(ii),PhiSym(Phi(ii),Isym));}
	BGL::vector3	dir(int ii) {return AnglesToDir3D(DegToRad(Phi(ii)), DegToRad(Theta(ii)));}
	double	Omega(int ii) {return omega[iTheta(ii)];}
	void	summary();

	int				nearestc(Double admax, BGL::vector3 dirext, vector<struct nearestdata>& nd);

};

struct btdfTrgz : public btdf
{
	int			Isym;
	vector<int>	DataIndx;
	Tregenza	Trgz0;

	btdfTrgz();

    HemiSphiral&        operator [] (int ii);

	int		size() {return Trgz0.NTrgz();}
	int	iiFindDataIndx(int iiTrgz);

	BGL::vector3	inDir(int ii);
	Double	inDirOmega(int ii) {return Trgz0.Omega(ii);}
	int		isym() {return Isym;}
	int		iisym(int ii) {return Trgz0.iiSym(ii,Isym);}
	int		iidata(int ii) {return DataIndx[ii];}
	Double	qinterp(BGL::vector3, BGL::vector3);
	Double	qexact(int iin, int jout);

	void	summary();
	ofstream&	save(ofstream&);
};
