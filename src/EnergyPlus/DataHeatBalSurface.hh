#ifndef DataHeatBalSurface_hh_INCLUDED
#define DataHeatBalSurface_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataHeatBalSurface {

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern Real64 const MinSurfaceTempLimit; // Lowest inside surface temperature allowed in Celsius
	extern Real64 const MinSurfaceTempLimitBeforeFatal; // 2.5 times MinSurfaceTempLimit
	extern Real64 const DefaultSurfaceTempLimit; // Highest inside surface temperature allowed in Celsius

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	// SUBROUTINE SPECIFICATIONS FOR MODULE DataHeatBalSurface
	//Integer Variables for the Heat Balance Simulation
	extern Array1D_int SUMH; // From Old Bldctf.inc

	//Variables Dimensioned to Max Number of Heat Transfer Surfaces (maxhts)
	extern Real64 MaxSurfaceTempLimit; // Highest inside surface temperature allowed in Celsius
	extern Real64 MaxSurfaceTempLimitBeforeFatal; // 2.5 times MaxSurfaceTempLimit
	extern Array1D< Real64 > CTFConstInPart; // Constant Inside Portion of the CTF calculation
	extern Array1D< Real64 > CTFConstOutPart; // Constant Outside Portion of the CTF calculation
	extern Array1D< Real64 > TempSurfIn; // Temperature of the Inside Surface for each heat transfer surface
	extern Array1D< Real64 > TempSurfInTmp; // Inside Surface Temperature Of Each Heat Transfer Surface
	extern Array1D< Real64 > HcExtSurf; // Outside Convection Coefficient
	extern Array1D< Real64 > HAirExtSurf; // Outside Convection Coefficient
	extern Array1D< Real64 > HSkyExtSurf; // Outside Convection Coefficient
	extern Array1D< Real64 > HGrdExtSurf; // Outside Convection Coefficient
	extern Array1D< Real64 > TempSource; // Temperature at the source location for each heat transfer surface
	extern Array1D< Real64 > TempSurfInRep; // Temperature of the Inside Surface for each heat transfer surface
	// (report)
	extern Array1D< Real64 > QConvInReport; // Surface convection heat gain at inside face [J]
	extern Array1D< Real64 > QdotConvInRep; // Surface convection heat transfer rate at inside face surface [W]
	// (report)
	extern Array1D< Real64 > QdotConvInRepPerArea; // Surface conv heat transfer rate per m2 at inside face surf
	//  (report){w/m2]

	//these next three all are for net IR thermal radiation exchange with other surfaces in the model.
	extern Array1D< Real64 > QRadNetSurfInReport; // Surface thermal radiation heat gain at Inside face [J]
	extern Array1D< Real64 > QdotRadNetSurfInRep; // Surface thermal radiation heat transfer inside face surface [W]
	extern Array1D< Real64 > QdotRadNetSurfInRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
	//      Inside face surf
	//these next three all are for solar radiation gains on inside face
	extern Array1D< Real64 > QRadSolarInReport; // Surface thermal radiation heat gain at Inside face [J]
	extern Array1D< Real64 > QdotRadSolarInRep; // Surface thermal radiation heat transfer inside face surface [W]
	extern Array1D< Real64 > QdotRadSolarInRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
	//      Inside face surf
	//these next three all are for Lights visible radiation gains on inside face
	extern Array1D< Real64 > QRadLightsInReport; // Surface thermal radiation heat gain at Inside face [J]
	extern Array1D< Real64 > QdotRadLightsInRep; // Surface thermal radiation heat transfer inside face surface [W]
	extern Array1D< Real64 > QdotRadLightsInRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
	//      Inside face surf
	//these next three all are for Internal Gains sources of radiation gains on inside face
	extern Array1D< Real64 > QRadIntGainsInReport; // Surface thermal radiation heat gain at Inside face [J]
	extern Array1D< Real64 > QdotRadIntGainsInRep; // Surface thermal radiation heat transfer inside face surface [W]
	extern Array1D< Real64 > QdotRadIntGainsInRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
	//      Inside face surf
	//these next three all are for Radiative HVAC sources of radiation gains on inside face
	extern Array1D< Real64 > QRadHVACInReport; // Surface thermal radiation heat gain at Inside face [J]
	extern Array1D< Real64 > QdotRadHVACInRep; // Surface thermal radiation heat transfer inside face surface [W]
	extern Array1D< Real64 > QdotRadHVACInRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
	//      Inside face surf

	extern Array1D< Real64 > QConvOutReport; // Surface convection heat gain at Outside face [J]
	extern Array1D< Real64 > QdotConvOutRep; // Surface convection heat transfer rate at Outside face surface [W]
	extern Array1D< Real64 > QdotConvOutRepPerArea; // Surface conv heat transfer rate per m2 at Outside face surf
	//  (report){w/m2]

	extern Array1D< Real64 > QRadOutReport; // Surface thermal radiation heat gain at Outside face [J]
	extern Array1D< Real64 > QdotRadOutRep; // Surface thermal radiation heat transfer outside face surface [W]
	extern Array1D< Real64 > QdotRadOutRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
	//      Outside face surf

	extern Array1D< Real64 > OpaqSurfInsFaceCondGainRep; // Equals Opaq Surf Ins Face Cond
	// when Opaq Surf Ins Face Cond >= 0
	extern Array1D< Real64 > OpaqSurfInsFaceCondLossRep; // Equals -Opaq Surf Ins Face Cond
	// when Opaq Surf Ins Face Cond  < 0
	extern Array1D< Real64 > OpaqSurfInsFaceConduction; // Opaque surface inside face heat conduction flow (W)
	// from inside of opaque surfaces, for reporting (W)
	extern Array1D< Real64 > OpaqSurfInsFaceConductionFlux; // Opaque surface inside face heat conduction flux (W/m2)
	// from inside of opaque surfaces, for reporting (W/m2)
	extern Array1D< Real64 > OpaqSurfInsFaceConductionEnergy; // Opaque surface inside face heat conduction flow (J)
	// from inside of opaque surfaces, for reporting (J)

	extern Array1D< Real64 > OpaqSurfExtFaceCondGainRep; // Equals Opaq Surf Ext Face Cond
	// when Opaq Surf Ext Face Cond >= 0
	extern Array1D< Real64 > OpaqSurfExtFaceCondLossRep; // Equals -Opaq Surf Ext Face Cond
	// when Opaq Surf Ext Face Cond  < 0
	extern Array1D< Real64 > OpaqSurfOutsideFaceConduction; // Opaque surface outside face heat conduction flow (W)
	// from inside of opaque surfaces, for reporting (W)
	extern Array1D< Real64 > OpaqSurfOutsideFaceConductionFlux; // Opaque surface outside face heat conduct flux (W/m2)
	// from outside of opaque surfaces, for reporting (W/m2)
	extern Array1D< Real64 > OpaqSurfOutsideFaceConductionEnergy; // Opaque surface outside face heat conduction flow (J)
	// from inside of opaque surfaces, for reporting (J)

	extern Array1D< Real64 > OpaqSurfAvgFaceCondGainRep; // Equals Opaq Surf average Face Cond
	// when Opaq Surf average Face Cond >= 0
	extern Array1D< Real64 > OpaqSurfAvgFaceCondLossRep; // Equals -Opaq Surf average Face Cond
	// when Opaq Surf average Face Cond  < 0
	extern Array1D< Real64 > OpaqSurfAvgFaceConduction; // Opaque surface average heat conduction flow (W)
	// net conduction from outside environ toward inside zone
	//  from inside of opaque surfaces, for reporting (W)
	extern Array1D< Real64 > OpaqSurfAvgFaceConductionFlux; // Opaque surface average face heat conduction flux (W/m2)
	// net conduction from outside environ to inside zone
	//  from inside of opaque surfaces, for reporting (W/m2)
	extern Array1D< Real64 > OpaqSurfAvgFaceConductionEnergy; // Opaque surface average heat conduction flow (J)
	// net conduction from outside environ toward inside zone
	//  from inside of opaque surfaces, for reporting (J)

	extern Array1D< Real64 > OpaqSurfStorageGainRep; // Equals Opaque surface stored heat conduction flow
	// when Opaque surface stored heat conduction flow  >= 0
	extern Array1D< Real64 > OpaqSurfStorageCondLossRep; // Equals -Opaque surface stored heat conduction flow
	// when Opaque surface stored heat conduction flow   < 0
	extern Array1D< Real64 > OpaqSurfStorageConduction; // Opaque surface stored heat conduction flow (W)
	// storage of heat inside surface, positive is increasing in surf
	extern Array1D< Real64 > OpaqSurfStorageConductionFlux; // Opaque surface stored heat conduction flux (W/m2)
	// storage of heat inside surface, positive is increasing in surf
	extern Array1D< Real64 > OpaqSurfStorageConductionEnergy; // Opaque surface stored heat conduction flow (J)
	// storage of heat inside surface, positive is increasing in surf

	extern Array1D< Real64 > OpaqSurfInsFaceBeamSolAbsorbed; // Opaque surface inside face absorbed beam solar,
	// for reporting (W)
	extern Array1D< Real64 > TempSurfOut; // Temperature of the Outside Surface for each heat transfer surface
	// used for reporting purposes only.  Ref: TH(x,1,1)
	extern Array1D< Real64 > QRadSWOutMvIns; // Short wave radiation absorbed on outside of movable insulation
	//unusedREAL(r64), ALLOCATABLE, DIMENSION(:) :: QBV                 !Beam solar absorbed by interior shades in a zone, plus
	// diffuse from beam not absorbed in zone, plus
	// beam absorbed at inside surfaces
	extern Array1D< Real64 > QC; // Short-Wave Radiation Converted Direct To Convection
	extern Array1D< Real64 > QD; // Diffuse solar radiation in a zone from sky and ground diffuse entering
	//through exterior windows and reflecting from interior surfaces,
	//beam from exterior windows reflecting from interior surfaces,
	//and beam entering through interior windows (considered diffuse)
	extern Array1D< Real64 > QDforDaylight; // Diffuse solar radiation in a zone from sky and ground diffuse entering
	//through exterior windows, beam from exterior windows reflecting
	//from interior surfaces, and beam entering through interior windows
	//(considered diffuse)
	//Originally QD, now used only for QSDifSol calc for daylighting
	extern Array1D< Real64 > QDV; // Diffuse solar radiation in a zone from sky and ground diffuse entering
	//through exterior windows
	extern Array1D< Real64 > TCONV; // Fraction Of Radiated Thermal Converted To Convection In Interior Shades
	extern Array1D< Real64 > VMULT; // 1/(Sum Of A Zone's Inside Surfaces Area*Absorptance)
	extern Array1D< Real64 > VCONV; // Fraction Of Short-Wave Radiation From Lights Converted To Convection
	extern Array1D< Real64 > NetLWRadToSurf; // Net interior long wavelength radiation to a surface from other surfaces
	extern Array1D< Real64 > ZoneMRT; // Zone Mean Radiant Temperature
	extern Array1D< Real64 > QRadSWLightsInAbs; // Short wave from Lights radiation absorbed on inside of opaque surface
	// Variables that are used in both the Surface Heat Balance and the Moisture Balance
	extern Array1D< Real64 > QRadSWOutAbs; // Short wave radiation absorbed on outside of opaque surface
	extern Array1D< Real64 > QRadSWInAbs; // Short wave radiation absorbed on inside of opaque surface

	extern Array1D< Real64 > InitialDifSolInAbs; // Initial diffuse solar absorbed on inside of opaque surface [W/m2]
	extern Array1D< Real64 > InitialDifSolInTrans; // Initial diffuse solar transmitted out through window surface [W/m2]

	//REAL(r64) variables from BLDCTF.inc and only used in the Heat Balance
	extern Array3D< Real64 > TH; // Temperature History (SurfNum,Hist Term,In/Out) where:
	//Hist Term (1 = Current Time, 2-MaxCTFTerms = previous times),
	//In/Out (1 = Outside, 2 = Inside)
	extern Array3D< Real64 > QH; // Flux History (TH and QH are interpolated from THM and QHM for
	//the next user requested time step)
	extern Array3D< Real64 > THM; // Master Temperature History (on the time step for the construct)
	extern Array3D< Real64 > QHM; // Master Flux History (on the time step for the construct)
	extern Array2D< Real64 > TsrcHist; // Temperature history at the source location (Term,SurfNum)
	extern Array2D< Real64 > QsrcHist; // Heat source/sink history for the surface (Term,SurfNum)
	extern Array2D< Real64 > TsrcHistM; // Master temperature history at the source location (Term,SurfNum)
	extern Array2D< Real64 > QsrcHistM; // Master heat source/sink history for the surface (Term,SurfNum)

	extern Array2D< Real64 > FractDifShortZtoZ; // Fraction of diffuse short radiation in Zone 2 transmitted to Zone 1
	extern Array1D_bool RecDifShortFromZ; // True if Zone gets short radiation from another
	extern bool InterZoneWindow; // True if there is an interzone window

	// Functions

	// Clears the global data in DataHeatBalSurface.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();


} // DataHeatBalSurface

} // EnergyPlus

#endif
