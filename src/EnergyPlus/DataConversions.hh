#ifndef DataConversions_hh_INCLUDED
#define DataConversions_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataConversions {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	//REAL(r64), PARAMETER:: CFC     =4.184D0            ! Specific Heat:  BTU/(LB*R) * CFC = KJ/(KG*K)
	extern Real64 const CFC; // Specific Heat:  BTU/(LB*R) * CFC = KJ/(KG*K)
	//  above is listed in July 1976 publication as "International Table"
	extern Real64 const CFL; // Length:         FT * CFL = M
	extern Real64 const CFM; // Mass:           LB * CFM = KG
	//REAL(r64), PARAMETER:: CFP     =249.082D0          ! Pressure:       IN-H2O * CFP = N/M**2
	// above is listed in July 1976 publication as in-water at 39.2 deg F
	extern Real64 const CFP; // Pressure:       IN-H2O * CFP = N/M**2
	//  above is listed in July 1976 publication as in-water at 60 deg F
	extern Real64 const DELTMP; // Temperature:    (F + DELTMP) * CFT = C
	extern Real64 const CFA; // Area:           FT**2 * CFA = M**2
	extern Real64 const CFT; // Temperature:    R * CFT = K
	extern Real64 const CFV; // Volume:         FT**3 * CFV = M**3
	extern Real64 const CFE; // Energy:         BTU * CFE = W-HR
	extern Real64 const CFD; // Density:        LB/FT**3 * CFD = KG/M**3
	extern Real64 const CFH; // Enthalpy:       BTU/LB * CFH = J/KG
	extern Real64 const CFK; // Conductivity:   BTU/(HR*FT*R) * CFK = W/(M*K)
	extern Real64 const CFMF; // Mass Flow:      LB/HR * CFMF = KG/SEC
	extern Real64 const CFQ; // Power:          BTU/HR * CFQ = W
	extern Real64 const CFU; // U-Value:        BTU/(HR*FT**2*R) * CFU = W/(M**2*K)
	// Note:  R-Value = 1/U-Value
	extern Real64 const CFS; // Speed:          FT/MIN * CFS = M/SEC
	extern Real64 const CFVF; // Volume Flow:    FT**3/MIN * CFVF = M**3/SEC
	extern Real64 const CFHF; // Heat Flux:      BTU/(HR*FT**2) * CFHF = W/M**2
	extern Real64 const CFTMP; // Temperature:    Same as DELTMP

} // DataConversions

} // EnergyPlus

#endif
