/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <math.h>
#include <cmath>
#include <limits>
#include "lib_sandia.h"

static double sandia_voc( double Tc, double Ee, double Voc0, double NcellSer, double DiodeFactor, double BVoc0, double mBVoc )
{
/*
C Returns Open-Circuit Voltage (V)
C Tc = cell temperature (deg C)
C Ee = effective irradiance
C Voco = Voc at SRC (1000 W/m2, 25 C) (V)
C NcellSer = # cells in series
C DiodeFactor = module-specIFic empirical constant
C BVoc0 = Voc temperature coefficient (V/C)
C mBVoc = change in BVoc with irradiance
*/
	if ( Ee > 0.0 ) 
	{
		double dTc = DiodeFactor*((1.38066E-23*(Tc + 273.15))/1.60218E-19);
		double BVocEe = BVoc0 + mBVoc * (1.0 - Ee);
  
		return Voc0+NcellSer*dTc*log(Ee)+BVocEe*(Tc-25.0);
	}
	else
		return 0.0;
}

static double sandia_vmp( double Tc, double Ee, double Vmp0, double NcellSer, double DiodeFactor, double BVmp0,
	double mBVmp, double C2, double C3 )
{
	/*
C Returns Voltage at Max. Power Point (V)
C Tc = cell temperature (deg C)
C Ee = effective irradiance
C Vmpo = Vmp at SRC (1000 W/m2, 25 C) (V)
C NcellSer = # cells in series
C DiodeFactor = module-specIFic empirical constant
C BVmp0 = Vmp temperature coefficient (V/C)
C mBVmp = change in BVmp with irradiance
C c2,c3 = empirical module-specIFic constants
*/
	if ( Ee > 0.0 )
	{
		double dTc=DiodeFactor*((1.38066E-23*(Tc+273.15))/1.60218E-19);
		double BVmpEe = BVmp0 + mBVmp * (1.0 - Ee);

		return Vmp0+C2*NcellSer*dTc*log(Ee)+C3*NcellSer*pow((dTc*log(Ee)),2)+BVmpEe*(Tc-25.0);
	}
	else
		return 0.0;
}
//
//static double sandia_ixx( double Tc, double Ee, double Ixx0, double aImp, double C6, double C7 )
//{
//	/*
//C Returns current "Ix" at V=0.5*(Voc+Vmp) (A)
//C Tc = cell temperature (deg C)
//C Ee = effective irradiance
//C Ixx0 = Ixx at SRC (1000 W/m2, 25 C) (A)
//C aImp = Imp temp coefficient (/C)
//C c4,c5 = empirical module-specIFic constants
//*/
//	return Ixx0*(C6*Ee+C7*Ee*Ee)*(1.0+aImp*(Tc-25.0));
//}
//
//static double sandia_ix(double Tc, double Ee, double Ix0, double aIsc, double aImp, double C4, double C5 )
//{
//	/*
//C Returns current "Ix" at V=0.5*Voc (A)
//C Tc = cell temperature (deg C)
//C Ee = effective irradiance
//C Ix0 = Ix at SRC (1000 W/m2, 25 C) (A)
//C aIsc = Isc temp coefficient (/C)
//C aImp = Imp temp coefficient (/C)
//C c4,c5 = empirical module-specIFic constants
//*/
//	return Ix0*(C4*Ee+C5*Ee*Ee)*(1.0+((aIsc+aImp)/2.0*(Tc-25.0)));
//}

static double sandia_isc( double Tc, double Isc0, double Ibc, double Idc, double F1, double F2, double fd, double aIsc, int radmode, double poaIrr )
{
	/*
C Returns Short-Circuit Current
C Isc0 = Isc at Tc=25 C, Ic=1000 W/m2 (A)
C Ibc  = beam radiation on collector plane (W/m2)
C Idc  = Diffuse radiation on collector plane (W/m2)
C F1   = Sandia F1 function of air mass
C F2   = Sandia F2 function of incidence angle
C fd   = module-specIFic empirical constant
C aIsc = Isc temperature coefficient (1/degC)
C Tc   = cell temperature */

// CNB UPDATED 12-3-07 BASED ON INFO FROM UPDATED CODE FROM GREG BARKER
// C       Isc0*((Ibc*F1*F2+fd*Idc)/1000.0)*(1.0+aIsc*(Tc-25.0))

//JMF updated 9/21/15 to account for POA input per email from Cliff Hansen on 9/21/15
// Cliff stated: "If POA is broadband use GPOA in place of f2(AOI)Eb + fd Ediff in the equation for Isc. If POA is a matched reference cell you also drop the F1(AMa) factor."
	double Isc;

	if (radmode == 3) //reference cell
		Isc = Isc0*(poaIrr / 1000.0)*(1.0 + aIsc*(Tc - 25.0)); //per Cliff: 
	else if (radmode == 4) //POA irradiance sensor ("broadband" in Cliff's email)
		Isc = Isc0*F1*(poaIrr / 1000.0)*(1.0 + aIsc*(Tc - 25.0));
	else
		Isc = Isc0*F1*((Ibc*F2+fd*Idc)/1000.0)*(1.0+aIsc*(Tc-25.0));

	return Isc;
}

static double sandia_imp(double Tc, double Ee, double Imp0, double aImp, double C0, double C1)
{ /*
C Returns current at maximum power point (A)
C Tc = cell temperature (degC)
C Ee = effective irradiance (W/m2)
C Imp0 = current at MPP at SRC (1000 W/m2, 25 C) (A)
C aImp = Imp temperature coefficient (degC^-1)
C c0,c1 = empirical module-specIFic constants */
	return Imp0*(C0*Ee+C1*Ee*Ee)*(1.0+aImp*(Tc-25.0));
}

static double sandia_f2( double IncAng, double b0, double b1, double b2, double b3, double b4, double b5 )
{
	/*
C Returns Sandia F2 function
C IncAng = incidence angle (deg)
C b0,b1,b2,b3,b4,b5 = empirical module-specIFic constants
*/

	double F2 = b0
		+ b1*IncAng
		+ b2*IncAng*IncAng
		+ b3*IncAng*IncAng*IncAng
		+ b4*IncAng*IncAng*IncAng*IncAng
		+ b5*IncAng*IncAng*IncAng*IncAng*IncAng;
  
	return F2 > 0.0 ? F2 : 0.0;
} 

static double sandia_f1( double AMa, double a0, double a1, double a2, double a3, double a4 )
{
	/*
C Returns the Sandia Air Mass function
C AMa = absolute air mass
C a0,a1,a2,a3,a4 = empirical constants, module-specIFic
*/
  
	double F1 = a0 
		+ a1*AMa 
		+ a2*AMa*AMa 
		+ a3*AMa*AMa*AMa 
		+ a4*AMa*AMa*AMa*AMa;

	return F1 > 0.0 ? F1 : 0.0;
} 

static double sandia_absolute_air_mass( double SolZen, double Altitude )
{
 /*
C Returns absolute air mass
C SolZen = solar zenith (deg)
C Altitude = site altitude (m)
*/
	if (SolZen < 89.9)
	{
		double AM = 1/(cos(SolZen * 0.01745329) + 0.5057 * pow( (96.08 - SolZen),-1.634));
		return AM * exp(-0.0001184 * Altitude);
	}
	else
		return 999;
}

static double sandia_effective_irradiance( double Tc, double Isc, double Isc0, double aIsc )
{
	/*
C Returns "effective irradiance", used in calculation of Imp, Voc, Ix, Ixx
C Tc   = cell temperature
C Isc = short-circuit current under operating conditions (A)
C Isc0 = Isc at Tc=25 C, Ic=1000 W/m2 (A)
C aIsc = Isc temperature coefficient (degC^-1) */
	return Isc / (1.0+aIsc*(Tc - 25.0))/Isc0;
}

static double sandia_current_at_voltage(double V, double VmaxPow, double ImaxPow, double Voc, double Isc)
{
/* from sam_sandia_pv_type701.for

	DOUBLE PRECISION TRW_Current
	DOUBLE PRECISION V,VmaxPow,ImaxPow,Voc,Isc
	DOUBLE PRECISION C_1,C_2,Itrw

        IF ((Isc.GT.0.).AND.(Voc.GT.0.)) THEN
	    IF(ImaxPow.LT.Isc) THEN
            C_2 = (VmaxPow / Voc - 1.0)/ Log(1.0 - ImaxPow / Isc)
	    ELSE
	      C_2 = 0.0
	    ENDIF
          IF (C_2.GT.0.) THEN
            C_1 = (1.0 - ImaxPow / Isc) * Exp(-VmaxPow / C_2 / Voc)

            Itrw = Isc*(1.0 - C_1 * (Exp(V / C_2 / Voc) - 1.0))
          ELSE
            Itrw = 0.0
          ENDIF
        ELSE
          Itrw = 0.0
        ENDIF

	  IF(Itrw.LT.0.0) THEN
	    Itrw=0.0
	  ENDIF

	  TRW_Current=Itrw
 */
	double Itrw=0,C_1=0,C_2=0;
	if ((Isc > 0) && (Voc > 0)) {
	    if(ImaxPow < Isc) C_2 = (VmaxPow / Voc - 1.0)/ log(1.0 - ImaxPow / Isc);

        if (C_2 > 0) {
            C_1 = (1.0 - ImaxPow / Isc) * exp(-VmaxPow / C_2 / Voc);
            Itrw = Isc*(1.0 - C_1 * (exp(V / C_2 / Voc) - 1.0));
		} else {
            Itrw = 0.0;
		}
	}
	if(Itrw < 0) Itrw=0;
	return Itrw;
}



sandia_module_t::sandia_module_t( )
{
	A0 = A1 = A2 = A3 = A4
		= B0 = B1 = B2 = B3 = B4 = B5
		= C0 = C1 = C2 = C3 = C4 = C5 = C6 = C7
		= Isc0 = aIsc
		= Imp0 = aImp
		= Voc0 = BVoc0 = mBVoc
		= Vmp0 = BVmp0 = mBVmp
		= Ix0 = Ixx0
		= fd = DiodeFactor = NcellSer
		= Area = std::numeric_limits<double>::quiet_NaN();
}


bool sandia_module_t::operator() ( pvinput_t &in, double TcellC, double opvoltage, pvoutput_t &out )
{
	
	out.Power = out.Voltage = out.Current = out.Efficiency = out.Voc_oper = out.Isc_oper = 0.0;
	out.CellTemp = TcellC;
	
	double Gtotal;
	if( in.radmode != 3 || !in.usePOAFromWF )
		Gtotal = in.Ibeam + in.Idiff + in.Ignd;
	else
		Gtotal = in.poaIrr;

	if ( Gtotal > 0.0 )
	{
		//C Calculate Air Mass
		double AMa = sandia_absolute_air_mass(in.Zenith, in.Elev);

		//C Calculate F1 function:
		double F1 = sandia_f1(AMa,A0,A1,A2,A3,A4);

		//C Calculate F2 function:
		double F2 = sandia_f2(in.IncAng,B0,B1,B2,B3,B4,B5);

		//C Calculate short-circuit current:
		double Isc = sandia_isc(TcellC,Isc0,in.Ibeam, in.Idiff+in.Ignd,F1,F2,fd,aIsc, in.radmode, Gtotal);

		//C Calculate effective irradiance:
		double Ee = sandia_effective_irradiance(TcellC,Isc,Isc0,aIsc);

		//C Calculate Imp:
		double Imp = sandia_imp(TcellC,Ee,Imp0,aImp,C0,C1);

		//C Calculate Voc:
		double Voc = sandia_voc(TcellC,Ee,Voc0,NcellSer,DiodeFactor,BVoc0,mBVoc);

		//C Calculate Vmp:
		double Vmp = sandia_vmp(TcellC,Ee,Vmp0,NcellSer,DiodeFactor,BVmp0,mBVmp,C2,C3);

		double V, I;
		if ( opvoltage < 0 )
		{
			V = Vmp;
			I = Imp;
		}
		else
		{		
			//C Calculate Ix:
//			double Ix = sandia_ix(TcellC,Ee,Ix0,aIsc,aImp,C4,C5);

			//C Calculate Vx:
//			double Vx = Voc/2.0;

			//C Calculate Ixx:
//			double Ixx = sandia_ixx(TcellC,Ee,Ixx0,aImp,C6,C7);

			//C Calculate Vxx:
//			double Vxx = 0.5*(Voc + Vmp);
			
			// calculate current at operating voltage
			V = opvoltage;
			
			// is this correct? (taken from SAM uicallback) (i.e. does not Ix, Ixx, etc)
			I = sandia_current_at_voltage( opvoltage, Vmp, Imp, Voc, Isc );
		}
	
		out.Power = V*I;
		out.Voltage = V;
		out.Current = I;
		out.Efficiency = I*V/(Gtotal*Area);
		out.AOIModifier = F2;
		out.Voc_oper = Voc;
		out.Isc_oper = Isc;
		out.CellTemp = TcellC;
	}
	
	return true;
}


sandia_inverter_t::sandia_inverter_t( )
{
	Paco = Pdco = Vdco = Pso
		= Pntare = C0 = C1 = C2 = C3 = std::numeric_limits<double>::quiet_NaN();
}

bool sandia_inverter_t::acpower(
	/* inputs */
	double Pdc,     /* Input power to inverter (Wdc) */
	double Vdc,     /* Voltage input to inverter (Vdc) */

	/* outputs */
	double *Pac,    /* AC output power (Wac) */
	double *Ppar,   /* AC parasitic power consumption (Wac) */
	double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
	double *Eff,	    /* Conversion efficiency (0..1) */
	double *Pcliploss, /* Power loss due to clipping loss (Wac) */
	double *Psoloss, /* Power loss due to operating power consumption (Wdc) */
	double *Pntloss /* Power loss due to night time tare loss (Wac) */
)
{
	//pass through inputs to the multiple MPPT function as an array with only one entry
	std::vector<double> Pdc_vec, Vdc_vec;
	Pdc_vec.push_back(Pdc);
	Vdc_vec.push_back(Vdc);
	if (!acpower(Pdc_vec, Vdc_vec, Pac, Ppar, Plr, Eff, Pcliploss, Psoloss, Pntloss))
		return false;

	return true;
}

bool sandia_inverter_t::acpower(
	/* inputs */
	std::vector<double> Pdc,     /* Input power to inverter (Wdc) */
	std::vector<double> Vdc,     /* Vector of Input power to inverter (Wdc), one per MPPT input on the inverter. Note that with several inverters, this is the power to ONE inverter.*/

	/* outputs */
	double *Pac,    /* AC output power (Wac) */
	double *Ppar,   /* AC parasitic power consumption (Wac) */
	double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
	double *Eff,	    /* Conversion efficiency (0..1) */
	double *Pcliploss, /* Power loss due to clipping loss (Wac) */
	double *Psoloss, /* Power loss due to operating power consumption (Wdc) */
	double *Pntloss /* Power loss due to night time tare loss (Wac) */
	)
{
	//initialize values
	*Pac = 0;
	*Ppar = 0.0;
	*Psoloss = 0.0; // Power consumption during operation
	*Pntloss = 0.0;
	*Pcliploss = 0.0;
	double Pdc_total = 0;
	std::vector<double> Pac_each;
	std::vector<double> PacNoPso_each;

	//loop through each MPPT input
	for (size_t m = 0; m < Pdc.size(); m++) 
	{
		Pac_each.push_back(0);
		PacNoPso_each.push_back(0);

		double A = Pdco * (1.0 + C1 * (Vdc[m] - Vdco));
		double B = Pso * (1.0 + C2 * (Vdc[m] - Vdco));
		double C = C0 * (1.0 + C3 * (Vdc[m] - Vdco));

		// crummy kludge to make sure B parameter has a resonable value (not negative!!)
		// even for inverters with weird input ranges and power levels, i.e. LeadSolar LS700
		// assumption is that Pso can't be less than half or more than double its nominal value
		if (B < 0.5 * Pso) B = 0.5 * Pso;
		if (B > 2.0 * Pso) B = 2.0 * Pso;

		Pac_each[m] = ((Paco / (A - B)) - C * (A - B)) * (Pdc[m] - B) + C0 * (Pdc[m] - B) * (Pdc[m] - B); //calculate Pac for this MPPT input and save it
		PacNoPso_each[m] = ((Paco / A) - C * A) * Pdc[m] + C0 * Pdc[m] * Pdc[m]; //calculate Pac without operating losses (Pso = 0) for each MPPT input to store as Pso losses later
		Pdc_total += Pdc[m];
	}

	// night time: power is equal to nighttime power loss (note that if PacNoPso > Pso and Pac < Pso then the night time loss could be considered an operating power loss)
	if (Pdc_total <= Pso)
	{
		*Pac = -Pntare;
		*Ppar = Pntare;
		*Pntloss = Pntare;
	}
	// day time: calculate total Pac; power loss is the Pso loss, use values calculated above
	else
		for (size_t m = 0; m < Vdc.size(); m++)
		{
			*Psoloss += PacNoPso_each[m] - Pac_each[m];
			*Pac += Pac_each[m];
		}
	
	// clipping loss Wac (note that the Pso=0 may have no clipping)
	double PacNoClip = *Pac;
	if ( *Pac > Paco )
	{
		*Pac = Paco;
		*Pcliploss = PacNoClip - *Pac;
	}

	*Plr = Pdc_total / Pdco;
	*Eff = *Pac / Pdc_total;
	if ( *Eff < 0.0 ) *Eff = 0.0;

	return true;
}


double sandia_celltemp_t::sandia_tcell_from_tmodule( double Tm, double poaIrr, double , double DT0)
{
	/*
C Returns cell temperature, deg C
C Tm  = module temperature (deg C)
C Ibc = beam radiation on collector plane, W/m2
C Idc = Diffuse radiation on collector plane, W/m2
C fd  = fraction of Idc used (empirical constant)
C DT0 = (Tc-Tm) at E=1000 W/m2 (empirical constant known as dTc), deg C
*/

//C Update from Chris Cameron - email 4/28/10  
//C        E = Ibc + fd * Idc
	double E = poaIrr;
	return Tm + E / 1000.0 * DT0;
}

double sandia_celltemp_t::sandia_module_temperature( double poaIrr, double Ws, double Ta, double , double a, double b )
{
	/*
C Returns back-of-module temperature, deg C
C Ibc = beam radiation on collector plane, W/m2
C Idc = Diffuse radiation on collector plane, W/m2
C Ws  = wind speed, m/s
C Ta  = ambient temperature, degC
C fd  = fraction of Idc used (empirical constant)
C a   = empirical constant
C b   = empirical constant
*/

//C Update from Chris Cameron - email 4/28/10  
//C        E = Ibc + fd * Idc
	double E = poaIrr;
	return E * exp(a + b * Ws) + Ta;
}

bool sandia_celltemp_t::operator() ( pvinput_t &input, pvmodule_t &, double , double &Tcell )
{
	//Sev 2015-09-14: changed to permit direct poa data
	double Itotal;
	if( input.radmode != 3 || !input.usePOAFromWF)
		Itotal = input.Ibeam + input.Idiff + input.Ignd;
	else
		Itotal = input.poaIrr;

	double tmod = sandia_module_temperature( Itotal, input.Wspd, input.Tdry, fd, a, b );
	Tcell = sandia_tcell_from_tmodule( tmod, Itotal, fd, DT0 );
	return true;
}
