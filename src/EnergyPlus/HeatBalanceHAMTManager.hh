#ifndef HeatBalanceHAMTManager_hh_INCLUDED
#define HeatBalanceHAMTManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace HeatBalanceHAMTManager {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const ittermax; // Maximum Number of itterations
	extern int const adjmax; // Maximum Number of Adjacent Cells

	extern Real64 const wdensity; // Density of water kg.m-3
	extern Real64 const wspech; // Specific Heat Capacity of Water J.kg-1.K-1 (at 20C)
	extern Real64 const whv; // Evaporation enthalpy of water J.kg-1
	extern Real64 const convt; // Temperature convergence limit
	extern Real64 const qvplim; // Maximum latent heat W
	extern Real64 const rhmax; // Maximum RH value

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_int firstcell;
	extern Array1D_int lastcell;
	extern Array1D_int Extcell;
	extern Array1D_int ExtRadcell;
	extern Array1D_int ExtConcell;
	extern Array1D_int ExtSkycell;
	extern Array1D_int ExtGrncell;
	extern Array1D_int Intcell;
	extern Array1D_int IntConcell;

	extern Array1D< Real64 > watertot;
	extern Array1D< Real64 > surfrh;
	extern Array1D< Real64 > surfextrh;
	extern Array1D< Real64 > surftemp;
	extern Array1D< Real64 > surfexttemp;
	extern Array1D< Real64 > surfvp;

	extern Array1D< Real64 > extvtc; // External Surface vapor transfer coefficient
	extern Array1D< Real64 > intvtc; // Internal Surface Vapor Transfer Coefficient
	extern Array1D_bool extvtcflag; // External Surface vapor transfer coefficient flag
	extern Array1D_bool intvtcflag; // Internal Surface Vapor Transfer Coefficient flag
	extern Array1D_bool MyEnvrnFlag; // Flag to reset surface properties.

	extern Real64 deltat; // time step in seconds

	extern int TotCellsMax; // Maximum number of cells per material

	extern bool latswitch; // latent heat switch,
	extern bool rainswitch; // rain switch,

	// SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceHAMTManager:

	// Types

	struct subcell
	{
		// Members
		int matid; // Material Id Number
		int sid; // Surface Id Number
		Real64 Qadds; // Additional sources of heat
		Real64 density; // Density
		Real64 wthermalc; // Moisture Dependant Thermal Conductivity
		Real64 spech; // Specific Heat capacity
		Real64 htc; // Heat Transfer Coefficient
		Real64 vtc; // Vapor Transfer Coefficient
		Real64 mu; // Vapor Diffusion resistance Factor
		Real64 volume; // Cell Volume
		Real64 temp;
		Real64 tempp1;
		Real64 tempp2;
		Real64 wreport; // Water content for reporting
		Real64 water; // Water Content of cells
		Real64 vp; // Vapor Pressure
		Real64 vpp1; // Vapor Pressure
		Real64 vpsat; // Saturation Vapor Pressure
		Real64 rh;
		Real64 rhp1;
		Real64 rhp2; // Relative Humidity
		Real64 rhp; // cell relative humidity (percent - reporting)
		Real64 dwdphi; // Moisture storage capacity
		Real64 dw; // Liquid transport Coefficient
		Array1D< Real64 > origin; // Cell origin. The geometric centre of the cell.
		Array1D< Real64 > length; // Cell lengths
		Array1D< Real64 > overlap; // Area of overlap
		Array1D< Real64 > dist; // distance between cell origins
		Array1D_int adjs;
		Array1D_int adjsl;

		// Default Constructor
		subcell() :
			matid( -1 ),
			sid( -1 ),
			Qadds( 0.0 ),
			density( -1.0 ),
			wthermalc( 0.0 ),
			spech( 0.0 ),
			htc( -1.0 ),
			vtc( -1.0 ),
			mu( -1.0 ),
			volume( 0.0 ),
			temp( 0.0 ),
			tempp1( 0.0 ),
			tempp2( 0.0 ),
			wreport( 0.0 ),
			water( 0.0 ),
			vp( 0.0 ),
			vpp1( 0.0 ),
			vpsat( 0.0 ),
			rh( 0.1 ),
			rhp1( 0.1 ),
			rhp2( 0.1 ),
			rhp( 10.0 ),
			dwdphi( -1.0 ),
			dw( -1.0 ),
			origin( 3, 0.0 ),
			length( 3, 0.0 ),
			overlap( 6, 0.0 ),
			dist( 6, 0.0 ),
			adjs( 6, 0 ),
			adjsl( 6, 0 )
		{}

		// Member Constructor
		subcell(
			int const matid, // Material Id Number
			int const sid, // Surface Id Number
			Real64 const Qadds, // Additional sources of heat
			Real64 const density, // Density
			Real64 const wthermalc, // Moisture Dependant Thermal Conductivity
			Real64 const spech, // Specific Heat capacity
			Real64 const htc, // Heat Transfer Coefficient
			Real64 const vtc, // Vapor Transfer Coefficient
			Real64 const mu, // Vapor Diffusion resistance Factor
			Real64 const volume, // Cell Volume
			Real64 const temp,
			Real64 const tempp1,
			Real64 const tempp2,
			Real64 const wreport, // Water content for reporting
			Real64 const water, // Water Content of cells
			Real64 const vp, // Vapor Pressure
			Real64 const vpp1, // Vapor Pressure
			Real64 const vpsat, // Saturation Vapor Pressure
			Real64 const rh,
			Real64 const rhp1,
			Real64 const rhp2, // Relative Humidity
			Real64 const rhp, // cell relative humidity (percent - reporting)
			Real64 const dwdphi, // Moisture storage capacity
			Real64 const dw, // Liquid transport Coefficient
			Array1< Real64 > const & origin, // Cell origin. The geometric centre of the cell.
			Array1< Real64 > const & length, // Cell lengths
			Array1< Real64 > const & overlap, // Area of overlap
			Array1< Real64 > const & dist, // distance between cell origins
			Array1_int const & adjs,
			Array1_int const & adjsl
		) :
			matid( matid ),
			sid( sid ),
			Qadds( Qadds ),
			density( density ),
			wthermalc( wthermalc ),
			spech( spech ),
			htc( htc ),
			vtc( vtc ),
			mu( mu ),
			volume( volume ),
			temp( temp ),
			tempp1( tempp1 ),
			tempp2( tempp2 ),
			wreport( wreport ),
			water( water ),
			vp( vp ),
			vpp1( vpp1 ),
			vpsat( vpsat ),
			rh( rh ),
			rhp1( rhp1 ),
			rhp2( rhp2 ),
			rhp( rhp ),
			dwdphi( dwdphi ),
			dw( dw ),
			origin( 3, origin ),
			length( 3, length ),
			overlap( 6, overlap ),
			dist( 6, dist ),
			adjs( 6, adjs ),
			adjsl( 6, adjsl )
		{}

	};

	// Object Data
	extern Array1D< subcell > cells;

	// Functions

	void
	ManageHeatBalHAMT(
		int const SurfNum,
		Real64 & TempSurfInTmp,
		Real64 & TempSurfOutTmp
	);

	void
	GetHeatBalHAMTInput();

	void
	InitHeatBalHAMT();

	void
	CalcHeatBalHAMT(
		int const sid,
		Real64 & TempSurfInTmp,
		Real64 & TempSurfOutTmp
	);

	void
	UpdateHeatBalHAMT( int const sid );

	void
	interp(
		int const ndata,
		Array1A< Real64 > const xx,
		Array1A< Real64 > const yy,
		Real64 const invalue,
		Real64 & outvalue,
		Optional< Real64 > outgrad = _
	);

	Real64
	RHtoVP(
		Real64 const RH,
		Real64 const Temperature
	);

	Real64
	WVDC(
		Real64 const Temperature,
		Real64 const ambp
	);

	//                                 COPYRIGHT NOTICE

	//     Portions Copyright ?University College London 2007.  All rights
	//     reserved.

	//     UCL LEGAL NOTICE
	//     Neither UCL, members of UCL nor any person or organisation acting on
	//     behalf of either:

	//     A. Makes any warranty of representation, express or implied with
	//        respect to the accuracy, completeness, or usefulness of the
	//        information contained in this program, including any warranty of
	//        merchantability or fitness of any purpose with respect to the
	//        program, or that the use of any information disclosed in this
	//        program may not infringe privately-owned rights, or

	//     B. Assumes any liability with respect to the use of, or for any and
	//        all damages resulting from the use of the program or any portion
	//        thereof or any information disclosed therein.

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of
	//     Illinois and The Regents of the University of California through
	//     Ernest Orlando Lawrence Berkeley National Laboratory.  All rights
	//     reserved.

	//     Portions of the EnergyPlus software package have been developed and
	//     copyrighted by other individuals, companies and institutions.  These
	//     portions have been incorporated into the EnergyPlus software package
	//     under license.  For a complete list of contributors, see "Notice"
	//     located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting
	//     on its behalf a paid-up, nonexclusive, irrevocable, worldwide license
	//     in this data to reproduce, prepare derivative works, and perform
	//     publicly and display publicly. Beginning five (5) years after
	//     permission to assert copyright is granted, subject to two possible
	//     five year renewals, the U.S. Government is granted for itself and
	//     others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display
	//     publicly, and to permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // HeatBalanceHAMTManager

} // EnergyPlus

#endif
