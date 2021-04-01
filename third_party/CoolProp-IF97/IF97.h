#ifndef IF97HEADER_H
#define IF97HEADER_H

#include <vector>
#include <cmath>
#include <algorithm>
#include <iostream>
#include <iomanip>      // std::setprecision
#include <stdexcept>
#include <stdio.h>

enum IF97parameters {IF97_DMASS, IF97_HMASS, IF97_T, IF97_P, IF97_SMASS, IF97_UMASS, IF97_CPMASS, IF97_CVMASS, IF97_W, IF97_DRHODP,
                    // Transport Property enumerations
                    IF97_MU, IF97_K };
                    
enum IF97SatState {NONE, LIQUID, VAPOR};   // Saturated Liquid/Vapor state determination

struct RegionIdealElement          // Structure for the single indexed state equation coefficients
{
    int J; ///< The first index
    double n; ///< The leading numerical constant
};
struct RegionResidualElement      // Structure for the double indexed state equation coefficients
{
    int I, ///< The first index
        J; ///< The second index
    double n; ///< The leading numerical constant
};

namespace IF97
{    
    // CoolProp-IF97 Version Number
    static char IF97VERSION [] = "v2.1.2";
    // Setup Water Constants for Trivial Functions and use in Region Classes
    // Constant values from:
    // Revised Release on the IAPWS Industrial Formulation 1997
    //     for the Thermodynamic Properties of Water and Steam, August 2007
    // IAPWS G5-01(2016), Guideline on the Use of Fundamental Physical Constants
    //      and Basic Constants of Water
#ifdef IAPWS_UNITS
    const double p_fact  = 1.0;                 // Leaves Thermodynamic Properties in IAPWS units of MPa
    const double R_fact  = 1.0;                 // Leaves Thermodynamic Properties in IAPWS units of kJ
#else
    const double p_fact  = 1e6;                 // Converts IAPWS MPa units to Pa
    const double R_fact  = 1000;                // Converts IAPWS kJ units to J
#endif
    // IF97 Constants
    const double Tcrit   = 647.096;             // K
    const double Pcrit   = 22.064*p_fact;       // Pa
    const double Rhocrit = 322.0;               // kg/m³
    const double Scrit   = 4.41202148223476*R_fact; // J/kg-K (needed for backward eqn. in Region 3(a)(b)
    const double Ttrip   = 273.16;              // K
    const double Ptrip   = 0.000611656*p_fact;  // Pa
    const double Tmin    = 273.15;              // K
    const double Tmax    = 1073.15;             // K
    const double Pmin    = 0.000611213*p_fact;  // Pa
    const double Pmax    = 100.0*p_fact;        // Pa
    const double Rgas    = 0.461526*R_fact;     // J/kg-K : mass based!
    const double MW      = 0.018015268;         // kg/mol
    // Bounds for Region Determination
    const double Text    = 2273.15;             // Extended (Region 5) Temperature Limit (Region 5) [K]
    const double Pext    = 50.0*p_fact;         // Extended (Region 5) Pressure Limit (Region 5) [Pa]
    const double P23min  = 16.529164252605*p_fact; // Min Pressure on Region23 boundary curve; Max is Pmax
    const double T23min  = 623.15;              // Min Temperature on Region23 boundary curve
    const double T23max  = 863.15;              // Max Temperature on Region23 boundary curve
    const double P2bcmin = 6.54670*p_fact;      // Min Pressure [MPa] on H2b2c boundary curve; Max is Pmax
    const double S2bc    = 5.85*R_fact;         // Min Pressure [MPa] on H2b2c boundary curve; Max is Pmax
    // Bounds for Backward p(h,s), t(h,s) Determination
    const double Smin    = 0.0;                         // Min Entropy [kJ/kg-K] for Backward p(h,s)
    const double Smax    = 11.921054825051103*R_fact;   // Max Entropy [kJ/kg-K] for Backward p(h,s)
    const double STPmax  = 6.04048367171238*R_fact;     // S(Tmax,Pmax)
    const double Sgtrip  = 9.155492076509681*R_fact;    // Sat. Vapor  Entropy [kJ/kg-K] at Triple Point
    const double Sftrip  = -4.09187776773977E-7*R_fact; // Sat. Liquid Entropy [kJ/kg-K] at Triple Point
    const double Hgtrip  = 2500.9109532932*R_fact;      // Sat. Vapor  Enthalpy [kJ/kg] at Triple Point
    const double Hftrip  = 5.16837786577998E-4*R_fact;  // Sat. Liquid Enthalpy [kJ/kg] at Triple Point
    const double SfT23   = 3.778281340*R_fact;          // Sat. Liquid Entropy [KJ/kg-K] at T23min
    const double SgT23   = 5.210887825*R_fact;          // Sat. Vapor  Entropy [KJ/kg-K] at T23min
    const double S13min  = 3.397782955*R_fact;          // Entropy at (T13,Pmax)
    const double S23min  = 5.048096828*R_fact;          // B23 Bounding Box
    const double S23max  = 5.260578707*R_fact;          // B23 Bounding Box
    const double H23min  = 2.563592004E3*R_fact;        // B23 Bounding Box
    const double H23max  = 2.812942061E3*R_fact;        // B23 Bounding Box
    //
    double Tsat97(double p);  // Forward declaration of Tsat97 required for calls below.
    double psat97(double T);  // Forward declaration of psat97 required for calls below.
    //

    static RegionResidualElement Hresiddata[] = {       // Residual H for viscosity
        {0, 0,  5.20094e-1},
        {1, 0,  8.50895e-2},
        {2, 0, -1.08374   },
        {3, 0, -2.89555e-1},
        {0, 1,  2.22531e-1},
        {1, 1,  9.99115e-1},
        {2, 1,  1.88797   },
        {3, 1,  1.26613   },
        {5, 1,  1.20573e-1},
        {0, 2, -2.81378e-1},
        {1, 2, -9.06851e-1},
        {2, 2, -7.72479e-1},
        {3, 2, -4.89837e-1},
        {4, 2, -2.57040e-1},
        {0, 3,  1.61913e-1},
        {1, 3,  2.57399e-1},
        {0, 4, -3.25372e-2},
        {3, 4,  6.98452e-2},
        {4, 5,  8.72102e-3},
        {3, 6, -4.35673e-3},
        {5, 6, -5.93264e-4}
    };

    static RegionIdealElement Hidealdata[] = {          // Ideal H for viscosity
        {0,  1.67752   },
        {1,  2.20462   },
        {2,  0.6366564 },
        {3, -0.241605  }
    };

    static RegionResidualElement Lresiddata[] = {       // Residual L for Thermal Conductivity
        { 0, 0,  1.60397357000 },
        { 1, 0,  2.33771842000 },
        { 2, 0,  2.19650529000 },
        { 3, 0, -1.21051378000 },
        { 4, 0, -2.72033700000 },
        { 0, 1, -0.64601352300 },
        { 1, 1, -2.78843778000 },
        { 2, 1, -4.54580785000 },
        { 3, 1,  1.60812989000 },
        { 4, 1,  4.57586331000 },
        { 0, 2,  0.11144390600 },
        { 1, 2,  1.53616167000 },
        { 2, 2,  3.55777244000 },
        { 3, 2, -0.62117814100 },
        { 4, 2, -3.18369245000 },
        { 0, 3,  0.10299735700 },
        { 1, 3, -0.46304551200 },
        { 2, 3, -1.40944978000 },
        { 3, 3,  0.07163732240 },
        { 4, 3,  1.11683480000 },
        { 0, 4, -0.05041236340 },
        { 1, 4,  0.08328270190 },
        { 2, 4,  0.27541827800 },
        { 3, 4,  0.00000000000 },
        { 4, 4, -0.19268305000 },
        { 0, 5,  0.00609859258 },
        { 1, 5, -0.00719201245 },
        { 2, 5, -0.02059388160 },
        { 3, 5,  0.00000000000 },
        { 4, 5,  0.01291384200 }
    };

    static RegionIdealElement Lidealdata[] = {          // Ideal L for thermal conductivity
        {0,  2.443221E-3},
        {1,  1.323095E-2},
        {2,  6.770357E-3},
        {3, -3.454586E-3},
        {4,  4.096266E-4}
    };

    static double A[6][5] = {
        {  6.53786807199516,  6.52717759281799,   5.35500529896124,   1.55225959906681,   1.11999926419994  },
        { -5.61149954923348, -6.30816983387575,  -3.96415689925446,   0.464621290821181,  0.595748562571649 },
        {  3.39624167361325,  8.08379285492595,   8.91990208918795,   8.93237374861479,   9.88952565078920  },
        { -2.27492629730878, -9.82240510197603, -12.03387295057900, -11.03219600611260, -10.32550511470400  },
        { 10.26318546627090, 12.13584137913950,   9.19494865194302,   6.16780999933360,   4.66861294457414  },
        {  1.97815050331519, -5.54349664571295,  -2.16866274479712,  -0.965458722086812, -0.503243546373828 },
    };

    static std::vector<RegionResidualElement> Hrdata(Hresiddata, Hresiddata + sizeof(Hresiddata)/sizeof(RegionResidualElement));
    static std::vector<RegionIdealElement> H0data(Hidealdata, Hidealdata + sizeof(Hidealdata)/sizeof(RegionIdealElement));
    static std::vector<RegionResidualElement> Lrdata(Lresiddata, Lresiddata + sizeof(Lresiddata)/sizeof(RegionResidualElement));
    static std::vector<RegionIdealElement> L0data(Lidealdata, Lidealdata + sizeof(Lidealdata)/sizeof(RegionIdealElement));

    class BaseRegion
    {
    public:
        BaseRegion(std::vector<RegionResidualElement> resid, std::vector<RegionIdealElement> ideal) : R(Rgas){
            for (std::size_t i = 0; i < resid.size(); ++i){
                nr.push_back(resid[i].n);
                Ir.push_back(resid[i].I);
                Jr.push_back(resid[i].J);
            }
            for (std::size_t i = 0; i < ideal.size(); ++i){
                n0.push_back(ideal[i].n);
                J0.push_back(ideal[i].J);
            }
            for (std::size_t i = 0; i < Hrdata.size(); ++i){
                munr.push_back(Hrdata[i].n);
                muIr.push_back(Hrdata[i].I);
                muJr.push_back(Hrdata[i].J);
            }
            for (std::size_t i = 0; i < H0data.size(); ++i){
                mun0.push_back(H0data[i].n);
                muJ0.push_back(H0data[i].J);
            }
            for (std::size_t i = 0; i < Lrdata.size(); ++i){
                lamnr.push_back(Lrdata[i].n);
                lamIr.push_back(Lrdata[i].I);
                lamJr.push_back(Lrdata[i].J);
            }
            for (std::size_t i = 0; i < L0data.size(); ++i){
                lamn0.push_back(L0data[i].n);
                lamJ0.push_back(L0data[i].J);
            }
        }
        double rhomass(double T, double p) const{
            return p_star/(R*T)/(p_fact/1000.0/R_fact)/(dgamma0_dPI(T,p) + dgammar_dPI(T,p));
        }
        double hmass(double T, double p) const{
            return R*T_star*(dgamma0_dTAU(T, p) + dgammar_dTAU(T, p));
        }
        double smass(double T, double p) const{
            const double tau = T_star/T;
            return R*(tau*(dgamma0_dTAU(T, p) + dgammar_dTAU(T, p)) - (gammar(T, p) + gamma0(T, p)));
        }
        double umass(double T, double p) const{
            const double tau = T_star/T, PI = p/p_star;
            return R*T*(tau*(dgamma0_dTAU(T, p) + dgammar_dTAU(T, p)) - PI*(dgamma0_dPI(T, p) + dgammar_dPI(T, p)));
        }
        double cpmass(double T, double p) const{
            const double tau = T_star/T;
            return -R*tau*tau*(d2gammar_dTAU2(T, p) + d2gamma0_dTAU2(T, p));
        }
        virtual double cvmass(double T, double p) const{
            const double tau = T_star/T, PI = p/p_star;
            return cpmass(T,p)-R*pow(1 + PI*dgammar_dPI(T,p) - tau*PI*d2gammar_dPIdTAU(T,p),2)/(1-PI*PI*d2gammar_dPI2(T, p));
        }
        virtual double speed_sound(double T, double p) const{
            const double tau = T_star/T, PI = p/p_star;
            const double RHS = (1 + 2*PI*dgammar_dPI(T,p) + PI*PI*pow(dgammar_dPI(T,p),2))/((1-PI*PI*d2gammar_dPI2(T,p)) +pow(1 + PI*dgammar_dPI(T,p) - tau*PI*d2gammar_dPIdTAU(T,p), 2)/(tau*tau*(d2gamma0_dTAU2(T,p) + d2gammar_dTAU2(T,p))));
            return sqrt(R*(1000/R_fact)*T*RHS);
        }
        double visc(double T, double rho) const{
            /// This base region function is valid for all IF97 regions since it is a function
            /// of density, not pressure, and can be called from any region instance.
            const double mu_star = 1.0E-6; // Reference viscosity [Pa-s]
            const double mu2 = 1.0;        // For Industrial Formulation (IF97), mu2 = 1.0
            return mu_star * mu0(T) * mu1(T,rho) * mu2;
        }
        double tcond(double T, double p, double rho) const{
            /// This base region function is valid for all IF97 regions 
            const double lambda_star = 0.001;  // Reference conductivity [W/m-K]
            const double lambda_bar = lambda0(T)*lambda1(T,rho) + lambda2(T,p,rho);
            return lambda_star * lambda_bar;
        }
        virtual double drhodp(double T, double p) const{
            /// Only valid for regions 2 and 5.  Will be overridden in Regions 1 and 3.
            /// Derived from IAPWS Revised Advisory Note No. 3 (See Table 2, Section 4.1 & 4.3)
            const double PI = p/p_star;
            return (rhomass(T,p)/p) * ( (1.0 - PI*PI*d2gammar_dPI2(T,p)) / (1.0 + PI*dgammar_dPI(T,p)) );
        }
        double delTr(double rho) const{
            /// This is the IF97 correlation for drhodp at the reducing temperature, Tr
            const double rhobar = rho/Rhocrit;
            double summer = 0;
            int j;
            //
            if      (rhobar <= 0.310559006) j = 0;
            else if (rhobar <= 0.776397516) j = 1;
            else if (rhobar <= 1.242236025) j = 2;
            else if (rhobar <= 1.863354037) j = 3;
            else                            j = 4;
            //
            for (int i=0; i < 6; i++)
                summer += A[i][j]*pow(rhobar,i);
            return 1.0/summer;
        }
        virtual double PIrterm(double) const = 0;
        virtual double TAUrterm(double) const = 0;
        virtual double TAU0term(double) const = 0;
        double output(IF97parameters key, double T, double p) const{
            switch(key){
            case IF97_T: return T;
            case IF97_P: return p;
            case IF97_DMASS: return rhomass(T, p);
            case IF97_HMASS: return hmass(T, p);
            case IF97_SMASS: return smass(T, p);
            case IF97_UMASS: return umass(T, p);
            case IF97_CPMASS: return cpmass(T, p);
            case IF97_CVMASS: return cvmass(T, p);
            case IF97_W: return speed_sound(T, p);
            case IF97_MU: return visc(T,rhomass(T,p));   // Viscosity is a function of rho.
            case IF97_K: return tcond(T,p,rhomass(T,p)); // Conductivity needs p and rho.
            case IF97_DRHODP: return drhodp(T, p);       // For verification testing.
            }
            throw std::out_of_range("Unable to match input parameters");
        }

    protected:
        std::vector<int> Ir, Jr;
        std::vector<double> nr;
        std::vector<int> J0;
        std::vector<double> n0;
        double T_star, p_star;
        const double R;
        /// For Viscosity Calculations
        std::vector<int> muJ0;
        std::vector<double> mun0;
        std::vector<int> muIr, muJr;
        std::vector<double> munr;
        /// For Thermal Conductivity Calculations
        std::vector<int> lamJ0;
        std::vector<double> lamn0;
        std::vector<int> lamIr, lamJr;
        std::vector<double> lamnr;
   
        double gammar(double T, double p) const{
            const double _PI = PIrterm(p), _TAU = TAUrterm(T);
            double summer = 0;
            for (std::size_t i = 0; i < Jr.size(); ++i){
                summer += nr[i]*pow(_PI, Ir[i])*pow(_TAU, Jr[i]);
            }
            return summer;
        }
        double dgammar_dPI(double T, double p) const{
            const double _PI = PIrterm(p), _TAU = TAUrterm(T);
            double summer = 0;
            for (std::size_t i = 0; i < Jr.size(); ++i){
                summer += nr[i]*Ir[i]*pow(_PI, Ir[i]-1)*pow(_TAU, Jr[i]);
            }
            return summer;
        }
        double d2gammar_dPI2(double T, double p) const{
            const double _PI = PIrterm(p), _TAU = TAUrterm(T);
            double summer = 0;
            for (std::size_t i = 0; i < Jr.size(); ++i){
                summer += nr[i]*Ir[i]*(Ir[i]-1)*pow(_PI, Ir[i]-2)*pow(_TAU, Jr[i]);
            }
            return summer;
        }
        double dgammar_dTAU(double T, double p) const{
            const double _PI = PIrterm(p), _TAU = TAUrterm(T);
            double summer = 0;
            for (std::size_t i = 0; i < Jr.size(); ++i){
                summer += nr[i]*Jr[i]*pow(_PI, Ir[i])*pow(_TAU, Jr[i]-1);
            }
            return summer;
        }
        double d2gammar_dPIdTAU(double T, double p) const{
            const double _PI = PIrterm(p), _TAU = TAUrterm(T);
            double summer = 0;
            for (std::size_t i = 0; i < Jr.size(); ++i){
                summer += nr[i]*Jr[i]*Ir[i]*pow(_PI, Ir[i]-1)*pow(_TAU, Jr[i]-1);
            }
            return summer;
        }
        double d2gammar_dTAU2(double T, double p) const {
            const double _PI = PIrterm(p), _TAU = TAUrterm(T);
            double summer = 0;
            for (std::size_t i = 0; i < Jr.size(); ++i){
                summer += nr[i]*Jr[i]*(Jr[i]-1)*pow(_PI, Ir[i])*pow(_TAU, Jr[i]-2);
            }
            return summer;
        }
        double gamma0(double T, double p) const{
            if (J0.size() == 0){ return 0; } // Region 1 has no term
            const double PI = p/p_star, _TAU = TAU0term(T);
            double summer = log(PI);
            for (std::size_t i = 0; i < n0.size(); ++i){
                summer += n0[i]*pow(_TAU, J0[i]);
            }
            return summer;
        }
        double dgamma0_dPI(double /*T*/, double p) const{
            if (J0.size() == 0){ return 0; } // Region 1 has no term
            const double PI = p/p_star;
            return 1.0/PI;
        }
        double d2gamma0_dPI2(double /*T*/, double p) const{
            if (J0.size() == 0){ return 0; } // Region 1 has no term
            const double PI = p/p_star;
            return -1.0/(PI*PI);
        }
        double dgamma0_dTAU(double T, double /*p*/) const{
            const double _TAU = TAU0term(T);
            double summer = 0;
            for (std::size_t i = 0; i < J0.size(); ++i){
                summer += n0[i]*J0[i]*pow(_TAU, J0[i]-1);
            }
            return summer;
        }
        double d2gamma0_dTAU2(double T, double /*p*/) const{
            const double _TAU = TAU0term(T);
            double summer = 0;
            for (std::size_t i = 0; i < J0.size(); ++i){
                summer += n0[i]*J0[i]*(J0[i]-1)*pow(_TAU, J0[i]-2);
            }
            return summer;
        }
        double mu0(double T) const{
            const double T_bar = T/Tcrit;
            double summer = 0.0;
            for (std::size_t i = 0; i < muJ0.size(); ++i){
                summer += mun0[i]/pow(T_bar, muJ0[i]);
            }
            return 100.0*sqrt(T_bar)/summer;
        }
        double mu1(double T, double rho) const{
            const double rho_bar = rho/Rhocrit;
            double summer = 0.0;
            for (std::size_t i = 0; i < muJr.size(); ++i){
                summer += rho_bar * pow(Trterm(T),muIr[i]) * munr[i]*pow(Rhorterm(rho),muJr[i]);
            }
            return exp(summer);
        }
        double lambda0(double T) const{
            const double T_bar = T/Tcrit;
            double summer = 0.0;
            for (std::size_t i = 0; i < lamJ0.size(); ++i){
                summer += lamn0[i]/pow(T_bar, lamJ0[i]);
            }
            return sqrt(T_bar)/summer;
        }
        double lambda1(double T, double rho) const{
            const double rho_bar = rho/Rhocrit;
            double summer = 0.0;
            for (std::size_t i = 0; i < lamJr.size(); ++i){
                summer += rho_bar * pow(Trterm(T),lamIr[i]) * lamnr[i]*pow(Rhorterm(rho),lamJr[i]);
            }
            return exp(summer);
        }
        double lambda2(double T, double p, double rho) const{
            double y, Cpbar, mubar, k, Z, delChi;
            const double rhobar = rho/Rhocrit;
            const double LAMBDA = 177.8514;
            const double qD     = 1.0/0.40;
            const double Tr     = 1.5*Tcrit;
            const double xi0    = 0.13;
            const double nu     = 0.630;
            const double gam    = 1.239;
            const double GAMMA0 = 0.06;
            const double PI     = 3.141592654;
            const double Cpstar = 0.46151805*R_fact;  /// Note: Slightly lower than IF97 Rgas
            Cpbar = cpmass(T,p)/Cpstar;
            if ((Cpbar < 0) || (Cpbar > 1.0E13)) Cpbar = 1.0E13;     /// Unit-less
            k = cpmass(T,p)/cvmass(T,p);
            mubar = visc(T,rho)/1.0E-6;
            delChi = rhobar*(Pcrit/Rhocrit*drhodp(T,p) - delTr(rho)*Tr/T);
            if (delChi > 0)                            /// At low (T,p), delChi can go negative, causing
                y = qD*xi0*pow(delChi/GAMMA0,nu/gam);  ///   y to be imaginary from this nth-root equation.
            else                                       ///   
                y = 0.0;                               ///   Limit delChi to > 0, values.
            if (y < 1.2E-7)                            /// Z is not calculated if y < 1.2E-7 since the
                Z = 0.0;                               ///   critical enhancement becomes insignificant.
            else
                Z = 2.0/PI/y*(((1.0-1.0/k)*atan(y)+y/k) - (1.0 - exp(-1.0/(1.0/y + y*y/(3.0*rhobar*rhobar)))));
            return LAMBDA*rhobar*Cpbar*T/(Tcrit*mubar)*Z;
        }
        double Trterm(double T) const{
            return Tcrit/T - 1.0;
        }
        double Rhorterm(double rho) const{
            return rho/Rhocrit - 1.0;
        }
    };
    

    /********************************************************************************/
    /**************************       Region #1       *******************************/
    /********************************************************************************/
    static RegionResidualElement Region1residdata[] = {
        // Note: the coefficients of n_i have been multiplied by -1**I_i such that all Gibbs terms are of the form (PI-7.1)**(I_i) rather than (7.1-PI)**(I_i)
        {0, -2, 0.14632971213167},
        {0, -1, -0.84548187169114},
        {0, 0, -3.756360367204},
        {0, 1, 3.3855169168385},
        {0, 2, -0.95791963387872},
        {0, 3, 0.15772038513228},
        {0, 4, -0.016616417199501},
        {0, 5, 0.00081214629983568},
        {1, -9, -0.00028319080123804},
        {1, -7, 0.00060706301565874},
        {1, -1, 0.018990068218419},
        {1, 0, 0.032529748770505},
        {1, 1, 0.021841717175414},
        {1, 3, 0.00005283835796993},
        {2, -3, -0.00047184321073267},
        {2, 0, -0.00030001780793026},
        {2, 1, 0.000047661393906987},
        {2, 3, -4.4141845330846E-06},
        {2, 17, -7.2694996297594E-16},
        {3, -4, 0.000031679644845054},
        {3, 0, 2.8270797985312E-06},
        {3, 6, 8.5205128120103E-10},
        {4, -5, -0.0000022425281908},
        {4, -2, -6.5171222895601E-07},
        {4, 10, -1.4341729937924E-13},
        {5, -8, 4.0516996860117E-07},
        {8, -11, -1.2734301741641E-09},
        {8, -6, -1.7424871230634E-10},
        {21, -29, 6.8762131295531E-19},
        {23, -31, -1.4478307828521E-20},
        {29, -38, -2.6335781662795E-23},
        {30, -39, -1.1947622640071E-23},
        {31, -40, -1.8228094581404E-24},
        {32, -41, -9.3537087292458E-26}
    };
    static std::vector<RegionResidualElement> reg1rdata(Region1residdata, Region1residdata + sizeof(Region1residdata)/sizeof(RegionResidualElement));
    static std::vector<RegionIdealElement> reg10data;
    class Region1 : public BaseRegion
    {
    public:
        Region1() : BaseRegion(reg1rdata, reg10data)  {
            T_star = 1386; p_star = 16.53*p_fact; 
        };    
        double speed_sound(double T, double p) const{
            // Evidently this formulation is special for some reason, and cannot be implemented using the base class formulation
			// see Table 3
            const double tau = T_star/T;
            const double RHS = pow(dgammar_dPI(T,p), 2)/(pow(dgammar_dPI(T,p)-tau*d2gammar_dPIdTAU(T,p), 2)/(tau*tau*d2gammar_dTAU2(T,p)) - d2gammar_dPI2(T, p));
            return sqrt(R*(1000/R_fact)*T*RHS);
        }
        double cvmass(double T, double p) const{
            // Evidently this formulation is special for some reason, and cannot be implemented using the base class formulation
            // see Table 3
            const double tau = T_star / T;
            return R*(-tau*tau*d2gammar_dTAU2(T,p) + pow(dgammar_dPI(T, p) - tau*d2gammar_dPIdTAU(T, p), 2) / d2gammar_dPI2(T, p));
        }
        double drhodp(double T, double p) const{
            //double PI = p/p_star;
            /// This one is different as well...
            /// Derived from IAPWS Revised Advisory Note No. 3 (See Table 2, Section 4.1 & 4.2)
            return -d2gammar_dPI2(T,p)/(pow(dgammar_dPI(T,p),2)*R*T)*(1000*R_fact/p_fact);
        }
        double TAUrterm(double T) const{
            return T_star/T - 1.222;
        }
        double PIrterm(double p) const{
            return p/p_star - 7.1;
        }
        double TAU0term(double /*T*/) const{
            return 0.0;
        }
    };


    /********************************************************************************/
    /**************************       Region #2       *******************************/
    /********************************************************************************/
    static RegionResidualElement Region2residdata[] = {
        {1,0,-0.0017731742473213},
        {1,1,-0.017834862292358},
        {1,2,-0.045996013696365},
        {1,3,-0.057581259083432},
        {1,6,-0.05032527872793},
        {2,1,-0.000033032641670203},
        {2,2,-0.00018948987516315},
        {2,4,-0.0039392777243355},
        {2,7,-0.043797295650573},
        {2,36,-0.000026674547914087},
        {3,0,2.0481737692309E-08},
        {3,1,4.3870667284435E-07},
        {3,3,-0.00003227767723857},
        {3,6,-0.0015033924542148},
        {3,35,-0.040668253562649},
        {4,1,-7.8847309559367E-10},
        {4,2,1.2790717852285E-08},
        {4,3,4.8225372718507E-07},
        {5,7,2.2922076337661E-06},
        {6,3,-1.6714766451061E-11},
        {6,16,-0.0021171472321355},
        {6,35,-23.895741934104},
        {7,0,-5.905956432427E-18},
        {7,11,-1.2621808899101E-06},
        {7,25,-0.038946842435739},
        {8,8,1.1256211360459E-11},
        {8,36,-8.2311340897998},
        {9,13,1.9809712802088E-08},
        {10,4,1.0406965210174E-19},
        {10,10,-1.0234747095929E-13},
        {10,14,-1.0018179379511E-09},
        {16,29,-8.0882908646985E-11},
        {16,50,0.10693031879409},
        {18,57,-0.33662250574171},
        {20,20,8.9185845355421E-25},
        {20,35,3.0629316876232E-13},
        {20,48,-4.2002467698208E-06},
        {21,21,-5.9056029685639E-26},
        {22,53,3.7826947613457E-06},
        {23,39,-1.2768608934681E-15},
        {24,26,7.3087610595061E-29},
        {24,40,5.5414715350778E-17},
        {24,58,-9.436970724121E-07}
    };

    static RegionIdealElement Region2idealdata[] = {
    {0, -0.96927686500217e1},
    {1, 0.10086655968018e2},
    {-5, -0.56087911283020e-2 },
    {-4, 0.71452738081455e-1},
    {-3, -0.40710498223928},
    {-2, 0.14240819171444e1},
    {-1, -0.43839511319450e1},
    {2, -0.28408632460772},
    {3, 0.21268463753307e-1},
    };
    static std::vector<RegionResidualElement> reg2rdata(Region2residdata, Region2residdata + sizeof(Region2residdata)/sizeof(RegionResidualElement));
    static std::vector<RegionIdealElement> reg20data(Region2idealdata, Region2idealdata + sizeof(Region2idealdata)/sizeof(RegionIdealElement));
    class Region2 : public BaseRegion
    {
    public:
        Region2() : BaseRegion(reg2rdata, reg20data)  {
            T_star = 540; p_star = 1*p_fact; 
        };
        double TAUrterm(double T) const{
            return T_star/T - 0.5;
        }
        double PIrterm(double p) const{
            return p/p_star;
        }
        double TAU0term(double T) const{
            return T_star/T;
        }
    };

    static double Region23data[] = {
    0.34805185628969e3,
    -0.11671859879975e1, 
    0.10192970039326e-2,
    0.57254459862746e3,
    0.13918839778870e2
    };

    static const std::vector<double> region23_n(Region23data, Region23data + sizeof(Region23data)/sizeof(double));

    inline double Region23_T(double T){
        const double p_star = 1*p_fact, T_star = 1, theta = T/T_star;
        const double PI = region23_n[0] + region23_n[1]*theta + region23_n[2]*theta*theta;
        return PI*p_star;
    }
    inline double Region23_p(double p){
        const double p_star = 1*p_fact, T_star = 1, PI = p/p_star;
        const double THETA = region23_n[3] + sqrt((PI - region23_n[4])/region23_n[2]);
        return THETA*T_star;
    }

    /********************************************************************************/
    /*********************       Region #3 (Backwards)      *************************/
    /********************* Implementation for v(T,p) only.  *************************/
    /********************************************************************************/

    namespace Region3Backwards{
        static RegionResidualElement Region3Adata[] = {
        {-12, 5, 0.110879558823853e-2},
        {-12, 10, 0.572616740810616e3},
        {-12, 12, -0.767051948380852e5},
        {-10, 5, -0.253321069529674e-1},
        {-10, 10, 0.628008049345689e4},
        {-10, 12, 0.234105654131876e6},
        {-8, 5, 0.216867826045856},
        {-8, 8, -0.156237904341963e3},
        {-8, 10, -0.269893956176613e5},
        {-6, 1, -0.180407100085505e-3},
        {-5, 1, 0.116732227668261e-2},
        {-5, 5, 0.266987040856040e2},
        {-5, 10, 0.282776617243286e5},
        {-4, 8, -0.242431520029523e4},
        {-3, 0, 0.435217323022733e-3},
        {-3, 1, -0.122494831387441e-1},
        {-3, 3, 0.179357604019989e1},
        {-3, 6, 0.442729521058314e2},
        {-2, 0, -0.593223489018342e-2},
        {-2, 2, 0.453186261685774},
        {-2, 3, 0.135825703129140e1},
        {-1, 0, 0.408748415856745e-1},
        {-1, 1, 0.474686397863312},
        {-1, 2, 0.118646814997915e1},
        {0, 0, 0.546987265727549},
        {0, 1, 0.195266770452643},
        {1, 0, -0.502268790869663e-1},
        {1, 2, -0.369645308193377},
        {2, 0, 0.633828037528420e-2},
        {2, 2, 0.797441793901017e-1},
        };

        static RegionResidualElement Region3Bdata[] = {
        {-12, 10, -0.827670470003621e-1},
        {-12, 12, 0.416887126010565e2},
        {-10, 8, 0.483651982197059e-1},
        {-10, 14, -0.291032084950276e5},
        {-8, 8, -0.111422582236948e3},
        {-6, 5, -0.202300083904014e-1},
        {-6, 6, 0.294002509338515e3},
        {-6, 8, 0.140244997609658e3},
        {-5, 5, -0.344384158811459e3},
        {-5, 8, 0.361182452612149e3},
        {-5, 10, -0.140699677420738e4},
        {-4, 2, -0.202023902676481e-2},
        {-4, 4, 0.171346792457471e3},
        {-4, 5, -0.425597804058632e1},
        {-3, 0, 0.691346085000334e-5},
        {-3, 1, 0.151140509678925e-2},
        {-3, 2, -0.416375290166236e-1},
        {-3, 3, -0.413754957011042e2},
        {-3, 5, -0.506673295721637e2},
        {-2, 0, -0.572212965569023e-3},
        {-2, 2, 0.608817368401785e1},
        {-2, 5, 0.239600660256161e2},
        {-1, 0, 0.122261479925384e-1},
        {-1, 2, 0.216356057692938e1},
        {0, 0, 0.398198903368642},
        {0, 1, -0.116892827834085},
        {1, 0, -0.102845919373532},
        {1, 2, -0.492676637589284},
        {2, 0, 0.655540456406790e-1},
        {3, 2, -0.240462535078530},
        {4, 0, -0.269798180310075e-1},
        {4, 1, 0.128369435967012},
        };

        static RegionResidualElement Region3Cdata[] = {
        {-12, 6, 3.11967788763030},
        {-12, 8, 2.76713458847564e+04},
        {-12, 10, 3.22583103403269e+07},
        {-10, 6, -3.42416065095363e+02},
        {-10, 8, -8.99732529907377e+05},
        {-10, 10, -7.93892049821251e+07},
        {-8, 5, 9.53193003217388e+01},
        {-8, 6, 2.29784742345072e+03},
        {-8, 7, 1.75336675322499e+05},
        {-6, 8, 7.91214365222792e+06},
        {-5, 1, 3.19933345844209e-05},
        {-5, 4, -6.59508863555767e+01},
        {-5, 7, -8.33426563212851e+05},
        {-4, 2, 6.45734680583292e-02},
        {-4, 8, -3.82031020570813e+06},
        {-3, 0, 4.06398848470079e-05},
        {-3, 3, 3.10327498492008e+01},
        {-2, 0, -8.92996718483724e-04},
        {-2, 4, 2.34604891591616e+02},
        {-2, 5, 3.77515668966951e+03},
        {-1, 0, 1.58646812591361e-02},
        {-1, 1, 7.07906336241843e-01},
        {-1, 2, 1.26016225146570e+01},
        {0, 0, 7.36143655772152e-01},
        {0, 1, 6.76544268999101e-01},
        {0, 2, -1.78100588189137e+01},
        {1, 0, -1.56531975531713e-01},
        {1, 2, 1.17707430048158e+01},
        {2, 0, 8.40143653860447e-02},
        {2, 1, -1.86442467471949e-01},
        {2, 3, -4.40170203949645e+01},
        {2, 7, 1.23290423502494e+06},
        {3, 0, -2.40650039730845e-02},
        {3, 7, -1.07077716660869e+06},
        {8, 1, 4.38319858566475e-02},
        };

        static RegionResidualElement Region3Ddata[] = {
        {-12, 4, -4.52484847171645e-10},
        {-12, 6, 3.15210389538801e-05},
        {-12, 7, -2.14991352047545e-03},
        {-12, 10, 5.08058874808345e+02},
        {-12, 12, -1.27123036845932e+07},
        {-12, 16, 1.15371133120497e+12},
        {-10, 0, -1.97805728776273e-16},
        {-10, 2, 2.41554806033972e-11},
        {-10, 4, -1.56481703640525e-06},
        {-10, 6, 2.77211346836625e-03},
        {-10, 8, -2.03578994462286e+01},
        {-10, 10, 1.44369489909053e+06},
        {-10, 14, -4.11254217946539e+10},
        {-8, 3, 6.23449786243773e-06},
        {-8, 7, -2.21774281146038e+01},
        {-8, 8, -6.89315087933158e+04},
        {-8, 10, -1.95419525060713e+07},
        {-6, 6, 3.16373510564015e+03},
        {-6, 8, 2.24040754426988e+06},
        {-5, 1, -4.36701347922356e-06},
        {-5, 2, -4.04213852833996e-04},
        {-5, 5, -3.48153203414663e+02},
        {-5, 7, -3.85294213555289e+05},
        {-4, 0, 1.35203700099403e-07},
        {-4, 1, 1.34648383271089e-04},
        {-4, 7, 1.25031835351736e+05},
        {-3, 2, 9.68123678455841e-02},
        {-3, 4, 2.25660517512438e+02},
        {-2, 0, -1.90102435341872e-04},
        {-2, 1, -2.99628410819229e-02},
        {-1, 0, 5.00833915372121e-03},
        {-1, 1, 3.87842482998411e-01},
        {-1, 5, -1.38535367777182e+03},
        {0, 0, 8.70745245971773e-01},
        {0, 2, 1.71946252068742},
        {1, 0, -3.26650121426383e-02},
        {1, 6, 4.98044171727877e+03},
        {3, 0, 5.51478022765087e-03},
        };

        static RegionResidualElement Region3Edata[] = {
        {-12, 14, 7.15815808404721e+08},
        {-12, 16, -1.14328360753449e+11},
        {-10, 3, 3.76531002015720e-12},
        {-10, 6, -9.03983668691157e-05},
        {-10, 10, 6.65695908836252e+05},
        {-10, 14, 5.35364174960127e+09},
        {-10, 16, 7.94977402335603e+10},
        {-8, 7, 9.22230563421437e+01},
        {-8, 8, -1.42586073991215e+05},
        {-8, 10, -1.11796381424162e+06},
        {-6, 6, 8.96121629640760e+03},
        {-5, 6, -6.69989239070491e+03},
        {-4, 2, 4.51242538486834e-03},
        {-4, 4, -3.39731325977713e+01},
        {-3, 2, -1.20523111552278},
        {-3, 6, 4.75992667717124e+04},
        {-3, 7, -2.66627750390341e+05},
        {-2, 0, -1.53314954386524e-04},
        {-2, 1, 3.05638404828265e-01},
        {-2, 3, 1.23654999499486e+02},
        {-2, 4, -1.04390794213011e+03},
        {-1, 0, -1.57496516174308e-02},
        {0, 0, 6.85331118940253e-01},
        {0, 1, 1.78373462873903},
        {1, 0, -5.44674124878910e-01},
        {1, 4, 2.04529931318843e+03},
        {1, 6, -2.28342359328752e+04},
        {2, 0, 4.13197481515899e-01},
        {2, 2, -3.41931835910405e+01},
        };

        static RegionResidualElement Region3Fdata[] = {
        {0, -3, -2.51756547792325e-08},
        {0, -2, 6.01307193668763e-06},
        {0, -1, -1.00615977450049e-03},
        {0, 0, 9.99969140252192e-01},
        {0, 1, 2.14107759236486},
        {0, 2, -1.65175571959086e+01},
        {1, -1, -1.41987303638727e-03},
        {1, 1, 2.69251915156554},
        {1, 2, 3.49741815858722e+01},
        {1, 3, -3.00208695771783e+01},
        {2, 0, -1.31546288252539},
        {2, 1, -8.39091277286169},
        {3, -5, 1.81545608337015e-10},
        {3, -2, -5.91099206478909e-04},
        {3, 0, 1.52115067087106},
        {4, -3, 2.52956470663225e-05},
        {5, -8, 1.00726265203786e-15},
        {5, 1, -1.49774533860650},
        {6, -6, -7.93940970562969e-10},
        {7, -4, -1.50290891264717e-04},
        {7, 1, 1.51205531275133},
        {10, -6, 4.70942606221652e-06},
        {12, -10, 1.95049710391712e-13},
        {12, -8, -9.11627886266077e-09},
        {12, -4, 6.04374640201265e-04},
        {14, -12, -2.25132933900136e-16},
        {14, -10, 6.10916973582981e-12},
        {14, -8, -3.03063908043404e-07},
        {14, -6, -1.37796070798409e-05},
        {14, -4, -9.19296736666106e-04},
        {16, -10, 6.39288223132545e-10},
        {16, -8, 7.53259479898699e-07},
        {18, -12, -4.00321478682929e-13},
        {18, -10, 7.56140294351614e-09},
        {20, -12, -9.12082054034891e-12},
        {20, -10, -2.37612381140539e-08},
        {20, -6, 2.69586010591874e-05},
        {22, -12, -7.32828135157839e-11},
        {24, -12, 2.41995578306660e-10},
        {24, -4, -4.05735532730322e-04},
        {28, -12, 1.89424143498011e-10},
        {32, -12, -4.86632965074563e-10},
        };

        static RegionResidualElement Region3Gdata[] = {
        {-12, 7, 4.12209020652996e-05},
        {-12, 12, -1.14987238280587e+06},
        {-12, 14, 9.48180885032080e+09},
        {-12, 18, -1.95788865718971e+17},
        {-12, 22, 4.96250704871300e+24},
        {-12, 24, -1.05549884548496e+28},
        {-10, 14, -7.58642165988278e+11},
        {-10, 20, -9.22172769596101e+22},
        {-10, 24, 7.25379072059348e+29},
        {-8, 7, -6.17718249205859e+01},
        {-8, 8, 1.07555033344858e+04},
        {-8, 10, -3.79545802336487e+07},
        {-8, 12, 2.28646846221831e+11},
        {-6, 8, -4.99741093010619e+06},
        {-6, 22, -2.80214310054101e+30},
        {-5, 7, 1.04915406769586e+06},
        {-5, 20, 6.13754229168619e+27},
        {-4, 22, 8.02056715528378e+31},
        {-3, 7, -2.98617819828065e+07},
        {-2, 3, -9.10782540134681e+01},
        {-2, 5, 1.35033227281565e+05},
        {-2, 14, -7.12949383408211e+18},
        {-2, 24, -1.04578785289542e+36},
        {-1, 2, 3.04331584444093e+01},
        {-1, 8, 5.93250797959445e+09},
        {-1, 18, -3.64174062110798e+27},
        {0, 0, 9.21791403532461e-01},
        {0, 1, -3.37693609657471e-01},
        {0, 2, -7.24644143758508e+01},
        {1, 0, -1.10480239272601e-01},
        {1, 1, 5.36516031875059},
        {1, 3, -2.91441872156205e+03},
        {3, 24, 6.16338176535305e+39},
        {5, 22, -1.20889175861180e+38},
        {6, 12, 8.18396024524612e+22},
        {8, 3, 9.40781944835829e+08},
        {10, 0, -3.67279669545448e+04},
        {10, 6, -8.37513931798655e+15},
        };

        static RegionResidualElement Region3Hdata[] = {
        {-12, 8, 5.61379678887577e-02},
        {-12, 12, 7.74135421587083e+09},
        {-10, 4, 1.11482975877938e-09},
        {-10, 6, -1.43987128208183e-03},
        {-10, 8, 1.93696558764920e+03},
        {-10, 10, -6.05971823585005e+08},
        {-10, 14, 1.71951568124337e+13},
        {-10, 16, -1.85461154985145e+16},
        {-8, 0, 3.87851168078010e-17},
        {-8, 1, -3.95464327846105e-14},
        {-8, 6, -1.70875935679023e+02},
        {-8, 7, -2.12010620701220e+03},
        {-8, 8, 1.77683337348191e+07},
        {-6, 4, 1.10177443629575e+01},
        {-6, 6, -2.34396091693313e+05},
        {-6, 8, -6.56174421999594e+06},
        {-5, 2, 1.56362212977396e-05},
        {-5, 3, -2.12946257021400},
        {-5, 4, 1.35249306374858e+01},
        {-4, 2, 1.77189164145813e-01},
        {-4, 4, 1.39499167345464e+03},
        {-3, 1, -7.03670932036388e-03},
        {-3, 2, -1.52011044389648e-01},
        {-2, 0, 9.81916922991113e-05},
        {-1, 0, 1.47199658618076e-03},
        {-1, 2, 2.02618487025578e+01},
        {0, 0, 8.99345518944240e-01},
        {1, 0, -2.11346402240858e-01},
        {1, 2, 2.49971752957491e+01},
        };

        static RegionResidualElement Region3Idata[] = {
        {0, 0, 1.06905684359136},
        {0, 1, -1.48620857922333},
        {0, 10, 2.59862256980408e+14},
        {1, -4, -4.46352055678749e-12},
        {1, -2, -5.66620757170032e-07},
        {1, -1, -2.35302885736849e-03},
        {1, 0, -2.69226321968839e-01},
        {2, 0, 9.22024992944392},
        {3, -5, 3.57633505503772e-12},
        {3, 0, -1.73942565562222e+01},
        {4, -3, 7.00681785556229e-06},
        {4, -2, -2.67050351075768e-04},
        {4, -1, -2.31779669675624},
        {5, -6, -7.53533046979752e-13},
        {5, -1, 4.81337131452891},
        {5, 12, -2.23286270422356e+21},
        {7, -4, -1.18746004987383e-05},
        {7, -3, 6.46412934136496e-03},
        {8, -6, -4.10588536330937e-10},
        {8, 10, 4.22739537057241e+19},
        {10, -8, 3.13698180473812e-13},
        {12, -12, 1.64395334345040e-24},
        {12, -6, -3.39823323754373e-06},
        {12, -4, -1.35268639905021e-02},
        {14, -10, -7.23252514211625e-15},
        {14, -8, 1.84386437538366e-09},
        {14, -4, -4.63959533752385e-02},
        {14, 5, -9.92263100376750e+13},
        {18, -12, 6.88169154439335e-17},
        {18, -10, -2.22620998452197e-11},
        {18, -8, -5.40843018624083e-08},
        {18, -6, 3.45570606200257e-03},
        {18, 2, 4.22275800304086e+10},
        {20, -12, -1.26974478770487e-15},
        {20, -10, 9.27237985153679e-10},
        {22, -12, 6.12670812016489e-14},
        {24, -12, -7.22693924063497e-12},
        {24, -8, -3.83669502636822e-04},
        {32, -10, 3.74684572410204e-04},
        {32, -5, -9.31976897511086e+04},
        {36, -10, -2.47690616026922e-02},
        {36, -8, 6.58110546759474e+01},
        };

        static RegionResidualElement Region3Jdata[] = {
        {0, -1, -1.11371317395540e-04},
        {0, 0, 1.00342892423685},
        {0, 1, 5.30615581928979},
        {1, -2, 1.79058760078792e-06},
        {1, -1, -7.28541958464774e-04},
        {1, 1, -1.87576133371704e+01},
        {2, -1, 1.99060874071849e-03},
        {2, 1, 2.43574755377290e+01},
        {3, -2, -1.77040785499444e-04},
        {4, -2, -2.59680385227130e-03},
        {4, 2, -1.98704578406823e+02},
        {5, -3, 7.38627790224287e-05},
        {5, -2, -2.36264692844138e-03},
        {5, 0, -1.61023121314333},
        {6, 3, 6.22322971786473e+03},
        {10, -6, -9.60754116701669e-09},
        {12, -8, -5.10572269720488e-11},
        {12, -3, 7.67373781404211e-03},
        {14, -10, 6.63855469485254e-15},
        {14, -8, -7.17590735526745e-10},
        {14, -5, 1.46564542926508e-05},
        {16, -10, 3.09029474277013e-12},
        {18, -12, -4.64216300971708e-16},
        {20, -12, -3.90499637961161e-14},
        {20, -10, -2.36716126781431e-10},
        {24, -12, 4.54652854268717e-12},
        {24, -6, -4.22271787482497e-03},
        {28, -12, 2.83911742354706e-11},
        {28, -5, 2.70929002720228},
        };

        static RegionResidualElement Region3Kdata[] = {
        {-2, 10, -4.01215699576099e+08},
        {-2, 12, 4.84501478318406e+10},
        {-1, -5, 3.94721471363678e-15},
        {-1, 6, 3.72629967374147e+04},
        {0, -12, -3.69794374168666e-30},
        {0, -6, -3.80436407012452e-15},
        {0, -2, 4.75361629970233e-07},
        {0, -1, -8.79148916140706e-04},
        {0, 0, 8.44317863844331e-01},
        {0, 1, 1.22433162656600e+01},
        {0, 2, -1.04529634830279e+02},
        {0, 3, 5.89702771277429e+02},
        {0, 14, -2.91026851164444e+13},
        {1, -3, 1.70343072841850e-06},
        {1, -2, -2.77617606975748e-04},
        {1, 0, -3.44709605486686},
        {1, 1, 2.21333862447095e+01},
        {1, 2, -1.94646110037079e+02},
        {2, -8, 8.08354639772825e-16},
        {2, -6, -1.80845209145470e-11},
        {2, -3, -6.96664158132412e-06},
        {2, -2, -1.81057560300994e-03},
        {2, 0, 2.55830298579027},
        {2, 4, 3.28913873658481e+03},
        {5, -12, -1.73270241249904e-19},
        {5, -6, -6.61876792558034e-07},
        {5, -3, -3.95688923421250e-03},
        {6, -12, 6.04203299819132e-18},
        {6, -10, -4.00879935920517e-14},
        {6, -8, 1.60751107464958e-09},
        {6, -5, 3.83719409025556e-05},
        {8, -12, -6.49565446702457e-15},
        {10, -12, -1.49095328506000e-12},
        {12, -10, 5.41449377329581e-09},
        };

        static RegionResidualElement Region3Ldata[] = {
        {-12, 14, 2.60702058647537e+09},
        {-12, 16, -1.88277213604704e+14},
        {-12, 18, 5.54923870289667e+18},
        {-12, 20, -7.58966946387758e+22},
        {-12, 22, 4.13865186848908e+26},
        {-10, 14, -8.15038000738060e+11},
        {-10, 24, -3.81458260489955e+32},
        {-8, 6, -1.23239564600519e-02},
        {-8, 10, 2.26095631437174e+07},
        {-8, 12, -4.95017809506720e+11},
        {-8, 14, 5.29482996422863e+15},
        {-8, 18, -4.44359478746295e+22},
        {-8, 24, 5.21635864527315e+34},
        {-8, 36, -4.87095672740742e+54},
        {-6, 8, -7.14430209937547e+05},
        {-5, 4, 1.27868634615495e-01},
        {-5, 5, -1.00752127917598e+01},
        {-4, 7, 7.77451437960990e+06},
        {-4, 16, -1.08105480796471e+24},
        {-3, 1, -3.57578581169659e-06},
        {-3, 3, -2.12857169423484},
        {-3, 18, 2.70706111085238e+29},
        {-3, 20, -6.95953622348829e+32},
        {-2, 2, 1.10609027472280e-01},
        {-2, 3, 7.21559163361354e+01},
        {-2, 10, -3.06367307532219e+14},
        {-1, 0, 2.65839618885530e-05},
        {-1, 1, 2.53392392889754e-02},
        {-1, 3, -2.14443041836579e+02},
        {0, 0, 9.37846601489667e-01},
        {0, 1, 2.23184043101700},
        {0, 2, 3.38401222509191e+01},
        {0, 12, 4.94237237179718e+20},
        {1, 0, -1.98068404154428e-01},
        {1, 16, -1.41415349881140e+30},
        {2, 1, -9.93862421613651e+01},
        {4, 0, 1.25070534142731e+02},
        {5, 0, -9.96473529004439e+02},
        {5, 1, 4.73137909872765e+04},
        {6, 14, 1.16662121219322e+32},
        {10, 4, -3.15874976271533e+15},
        {10, 12, -4.45703369196945e+32},
        {14, 10, 6.42794932373694e+32},
        };

        static RegionResidualElement Region3Mdata[] = {
        {0, 0, 8.11384363481847e-01},
        {3, 0, -5.68199310990094e+03},
        {8, 0, -1.78657198172556e+10},
        {20, 2, 7.95537657613427e+31},
        {1, 5, -8.14568209346872e+04},
        {3, 5, -6.59774567602874e+07},
        {4, 5, -1.52861148659302e+10},
        {5, 5, -5.60165667510446e+11},
        {1, 6, 4.58384828593949e+05},
        {6, 6, -3.85754000383848e+13},
        {2, 7, 4.53735800004273e+07},
        {4, 8, 9.39454935735563e+11},
        {14, 8, 2.66572856432938e+27},
        {2, 10, -5.47578313899097e+09},
        {5, 10, 2.00725701112386e+14},
        {3, 12, 1.85007245563239e+12},
        {0, 14, 1.85135446828337e+08},
        {1, 14, -1.70451090076385e+11},
        {1, 18, 1.57890366037614e+14},
        {1, 20, -2.02530509748774e+15},
        {28, 20, 3.68193926183570e+59},
        {2, 22, 1.70215539458936e+17},
        {16, 22, 6.39234909918741e+41},
        {0, 24, -8.21698160721956e+14},
        {5, 24, -7.95260241872306e+23},
        {0, 28, 2.33415869478510e+17},
        {3, 28, -6.00079934586803e+22},
        {4, 28, 5.94584382273384e+24},
        {12, 28, 1.89461279349492e+39},
        {16, 28, -8.10093428842645e+45},
        {1, 32, 1.88813911076809e+21},
        {8, 32, 1.11052244098768e+35},
        {14, 32, 2.91133958602503e+45},
        {0, 36, -3.29421923951460e+21},
        {2, 36, -1.37570282536696e+25},
        {3, 36, 1.81508996303902e+27},
        {4, 36, -3.46865122768353e+29},
        {8, 36, -2.11961148774260e+37},
        {14, 36, -1.28617899887675e+48},
        {24, 36, 4.79817895699239e+64},
        };

        static RegionResidualElement Region3Ndata[] = {
        {0, -12, 2.80967799943151e-39},
        {3, -12, 6.14869006573609e-31},
        {4, -12, 5.82238667048942e-28},
        {6, -12, 3.90628369238462e-23},
        {7, -12, 8.21445758255119e-21},
        {10, -12, 4.02137961842776e-15},
        {12, -12, 6.51718171878301e-13},
        {14, -12, -2.11773355803058e-08},
        {18, -12, 2.64953354380072e-03},
        {0, -10, -1.35031446451331e-32},
        {3, -10, -6.07246643970893e-24},
        {5, -10, -4.02352115234494e-19},
        {6, -10, -7.44938506925544e-17},
        {8, -10, 1.89917206526237e-13},
        {12, -10, 3.64975183508473e-06},
        {0, -8, 1.77274872361946e-26},
        {3, -8, -3.34952758812999e-19},
        {7, -8, -4.21537726098389e-09},
        {12, -8, -3.91048167929649e-02},
        {2, -6, 5.41276911564176e-14},
        {3, -6, 7.05412100773699e-12},
        {4, -6, 2.58585887897486e-09},
        {2, -5, -4.93111362030162e-11},
        {4, -5, -1.58649699894543e-06},
        {7, -5, -5.25037427886100e-01},
        {4, -4, 2.20019901729615e-03},
        {3, -3, -6.43064132636925e-03},
        {5, -3, 6.29154149015048e+01},
        {6, -3, 1.35147318617061e+02},
        {0, -2, 2.40560808321713e-07},
        {0, -1, -8.90763306701305e-04},
        {3, -1, -4.40209599407714e+03},
        {1, 0, -3.02807107747776e+02},
        {0, 1, 1.59158748314599e+03},
        {1, 1, 2.32534272709876e+05},
        {0, 2, -7.92681207132600e+05},
        {1, 4, -8.69871364662769e+10},
        {0, 5, 3.54542769185671e+11},
        {1, 6, 4.00849240129329e+14},
        };

        static RegionResidualElement Region3Odata[] = {
        {0, -12, 1.28746023979718e-35},
        {0, -4, -7.35234770382342e-12},
        {0, -1, 2.89078692149150e-03},
        {2, -1, 2.44482731907223e-01},
        {3, -10, 1.41733492030985e-24},
        {4, -12, -3.54533853059476e-29},
        {4, -8, -5.94539202901431e-18},
        {4, -5, -5.85188401782779e-09},
        {4, -4, 2.01377325411803e-06},
        {4, -1, 1.38647388209306},
        {5, -4, -1.73959365084772e-05},
        {5, -3, 1.37680878349369e-03},
        {6, -8, 8.14897605805513e-15},
        {7, -12, 4.25596631351839e-26},
        {8, -10, -3.87449113787755e-18},
        {8, -8, 1.39814747930240e-13},
        {8, -4, -1.71849638951521e-03},
        {10, -12, 6.41890529513296e-22},
        {10, -8, 1.18960578072018e-11},
        {14, -12, -1.55282762571611e-18},
        {14, -8, 2.33907907347507e-08},
        {20, -12, -1.74093247766213e-13},
        {20, -10, 3.77682649089149e-09},
        {24, -12, -5.16720236575302e-11},
        };

        static RegionResidualElement Region3Pdata[] = {
        {0, -1, -9.82825342010366e-05},
        {0, 0, 1.05145700850612},
        {0, 1, 1.16033094095084e+02},
        {0, 2, 3.24664750281543e+03},
        {1, 1, -1.23592348610137e+03},
        {2, -1, -5.61403450013495e-02},
        {3, -3, 8.56677401640869e-08},
        {3, 0, 2.36313425393924e+02},
        {4, -2, 9.72503292350109e-03},
        {6, -2, -1.03001994531927},
        {7, -5, -1.49653706199162e-09},
        {7, -4, -2.15743778861592e-05},
        {8, -2, -8.34452198291445},
        {10, -3, 5.86602660564988e-01},
        {12, -12, 3.43480022104968e-26},
        {12, -6, 8.16256095947021e-06},
        {12, -5, 2.94985697916798e-03},
        {14, -10, 7.11730466276584e-17},
        {14, -8, 4.00954763806941e-10},
        {14, -3, 1.07766027032853e+01},
        {16, -8, -4.09449599138182e-07},
        {18, -8, -7.29121307758902e-06},
        {20, -10, 6.77107970938909e-09},
        {22, -10, 6.02745973022975e-08},
        {24, -12, -3.82323011855257e-11},
        {24, -8, 1.79946628317437e-03},
        {36, -12, -3.45042834640005e-04},
        };

        static RegionResidualElement Region3Qdata[] = {
        {-12, 10, -8.20433843259950e+04},
        {-12, 12, 4.73271518461586e+10},
        {-10, 6, -8.05950021005413e-02},
        {-10, 7, 3.28600025435980e+01},
        {-10, 8, -3.56617029982490e+03},
        {-10, 10, -1.72985781433335e+09},
        {-8, 8, 3.51769232729192e+07},
        {-6, 6, -7.75489259985144e+05},
        {-5, 2, 7.10346691966018e-05},
        {-5, 5, 9.93499883820274e+04},
        {-4, 3, -6.42094171904570e-01},
        {-4, 4, -6.12842816820083e+03},
        {-3, 3, 2.32808472983776e+02},
        {-2, 0, -1.42808220416837e-05},
        {-2, 1, -6.43596060678456e-03},
        {-2, 2, -4.28577227475614},
        {-2, 4, 2.25689939161918e+03},
        {-1, 0, 1.00355651721510e-03},
        {-1, 1, 3.33491455143516e-01},
        {-1, 2, 1.09697576888873},
        {0, 0, 9.61917379376452e-01},
        {1, 0, -8.38165632204598e-02},
        {1, 1, 2.47795908411492},
        {1, 3, -3.19114969006533e+03},
        };

        static RegionResidualElement Region3Rdata[] = {
        {-8, 6, 1.44165955660863e-03},
        {-8, 14, -7.01438599628258e+12},
        {-3, -3, -8.30946716459219e-17},
        {-3, 3, 2.61975135368109e-01},
        {-3, 4, 3.93097214706245e+02},
        {-3, 5, -1.04334030654021e+04},
        {-3, 8, 4.90112654154211e+08},
        {0, -1, -1.47104222772069e-04},
        {0, 0, 1.03602748043408},
        {0, 1, 3.05308890065089},
        {0, 5, -3.99745276971264e+06},
        {3, -6, 5.69233719593750e-12},
        {3, -2, -4.64923504407778e-02},
        {8, -12, -5.35400396512906e-18},
        {8, -10, 3.99988795693162e-13},
        {8, -8, -5.36479560201811e-07},
        {8, -5, 1.59536722411202e-02},
        {10, -12, 2.70303248860217e-15},
        {10, -10, 2.44247453858506e-08},
        {10, -8, -9.83430636716454e-06},
        {10, -6, 6.63513144224454e-02},
        {10, -5, -9.93456957845006},
        {10, -4, 5.46491323528491e+02},
        {10, -3, -1.43365406393758e+04},
        {10, -2, 1.50764974125511e+05},
        {12, -12, -3.37209709340105e-10},
        {14, -12, 3.77501980025469e-09},
        };

        static RegionResidualElement Region3Sdata[] = {
        {-12, 20, -5.32466612140254e+22},
        {-12, 24, 1.00415480000824e+31},
        {-10, 22, -1.91540001821367e+29},
        {-8, 14, 1.05618377808847e+16},
        {-6, 36, 2.02281884477061e+58},
        {-5, 8, 8.84585472596134e+07},
        {-5, 16, 1.66540181638363e+22},
        {-4, 6, -3.13563197669111e+05},
        {-4, 32, -1.85662327545324e+53},
        {-3, 3, -6.24942093918942e-02},
        {-3, 8, -5.04160724132590e+09},
        {-2, 4, 1.87514491833092e+04},
        {-1, 1, 1.21399979993217e-03},
        {-1, 2, 1.88317043049455},
        {-1, 3, -1.67073503962060e+03},
        {0, 0, 9.65961650599775e-01},
        {0, 1, 2.94885696802488},
        {0, 4, -6.53915627346115e+04},
        {0, 28, 6.04012200163444e+49},
        {1, 0, -1.98339358557937e-01},
        {1, 32, -1.75984090163501e+57},
        {3, 0, 3.56314881403987},
        {3, 1, -5.75991255144384e+02},
        {3, 2, 4.56213415338071e+04},
        {4, 3, -1.09174044987829e+07},
        {4, 18, 4.37796099975134e+33},
        {4, 24, -6.16552611135792e+45},
        {5, 4, 1.93568768917797e+09},
        {14, 24, 9.50898170425042e+53},
        };

        static RegionResidualElement Region3Tdata[] = {
        {0, 0, 1.55287249586268},
        {0, 1, 6.64235115009031},
        {0, 4, -2.89366236727210e+03},
        {0, 12, -3.85923202309848e+12},
        {1, 0, -2.91002915783761},
        {1, 10, -8.29088246858083e+11},
        {2, 0, 1.76814899675218},
        {2, 6, -5.34686695713469e+08},
        {2, 14, 1.60464608687834e+17},
        {3, 3, 1.96435366560186e+05},
        {3, 8, 1.56637427541729e+12},
        {4, 0, -1.78154560260006},
        {4, 10, -2.29746237623692e+15},
        {7, 3, 3.85659001648006e+07},
        {7, 4, 1.10554446790543e+09},
        {7, 7, -6.77073830687349e+13},
        {7, 20, -3.27910592086523e+30},
        {7, 36, -3.41552040860644e+50},
        {10, 10, -5.27251339709047e+20},
        {10, 12, 2.45375640937055e+23},
        {10, 14, -1.68776617209269e+26},
        {10, 16, 3.58958955867578e+28},
        {10, 22, -6.56475280339411e+35},
        {18, 18, 3.55286045512301e+38},
        {20, 32, 5.69021454413270e+57},
        {22, 22, -7.00584546433113e+47},
        {22, 36, -7.05772623326374e+64},
        {24, 24, 1.66861176200148e+52},
        {28, 28, -3.00475129680486e+60},
        {32, 22, -6.68481295196808e+50},
        {32, 32, 4.28432338620678e+68},
        {32, 36, -4.44227367758304e+71},
        {36, 36, -2.81396013562745e+76},
        };

        static RegionResidualElement Region3Udata[] = {
        {-12, 14, 1.22088349258355e+17},
        {-10, 10, 1.04216468608488e+09},
        {-10, 12, -8.82666931564652e+15},
        {-10, 14, 2.59929510849499e+19},
        {-8, 10, 2.22612779142211e+14},
        {-8, 12, -8.78473585050085e+17},
        {-8, 14, -3.14432577551552e+21},
        {-6, 8, -2.16934916996285e+12},
        {-6, 12, 1.59079648196849e+20},
        {-5, 4, -3.39567617303423e+02},
        {-5, 8, 8.84387651337836e+12},
        {-5, 12, -8.43405926846418e+20},
        {-3, 2, 1.14178193518022e+01},
        {-1, -1, -1.22708229235641e-04},
        {-1, 1, -1.06201671767107e+02},
        {-1, 12, 9.03443213959313e+24},
        {-1, 14, -6.93996270370852e+27},
        {0, -3, 6.48916718965575e-09},
        {0, 1, 7.18957567127851e+03},
        {1, -2, 1.05581745346187e-03},
        {2, 5, -6.51903203602581e+14},
        {2, 10, -1.60116813274676e+24},
        {3, -5, -5.10254294237837e-09},
        {5, -4, -1.52355388953402e-01},
        {5, 2, 6.77143292290144e+11},
        {5, 3, 2.76378438378930e+14},
        {6, -5, 1.16862983141686e-02},
        {6, 2, -3.01426947980171e+13},
        {8, -8, 1.69719813884840e-08},
        {8, 8, 1.04674840020929e+26},
        {10, -4, -1.08016904560140e+04},
        {12, -12, -9.90623601934295e-13},
        {12, -4, 5.36116483602738e+06},
        {12, 4, 2.26145963747881e+21},
        {14, -12, -4.88731565776210e-10},
        {14, -10, 1.51001548880670e-05},
        {14, -6, -2.27700464643920e+04},
        {14, 6, -7.81754507698846e+27},
        };

        static RegionResidualElement Region3Vdata[] = {
        {-10, -8, -4.15652812061591e-55},
        {-8, -12, 1.77441742924043e-61},
        {-6, -12, -3.57078668203377e-55},
        {-6, -3, 3.59252213604114e-26},
        {-6, 5, -2.59123736380269e+01},
        {-6, 6, 5.94619766193460e+04},
        {-6, 8, -6.24184007103158e+10},
        {-6, 10, 3.13080299915944e+16},
        {-5, 1, 1.05006446192036e-09},
        {-5, 2, -1.92824336984852e-06},
        {-5, 6, 6.54144373749937e+05},
        {-5, 8, 5.13117462865044e+12},
        {-5, 10, -6.97595750347391e+18},
        {-5, 14, -1.03977184454767e+28},
        {-4, -12, 1.19563135540666e-48},
        {-4, -10, -4.36677034051655e-42},
        {-4, -6, 9.26990036530639e-30},
        {-4, 10, 5.87793105620748e+20},
        {-3, -3, 2.80375725094731e-18},
        {-3, 10, -1.92359972440634e+22},
        {-3, 12, 7.42705723302738e+26},
        {-2, 2, -5.17429682450605e+01},
        {-2, 4, 8.20612048645469e+06},
        {-1, -2, -1.88214882341448e-09},
        {-1, 0, 1.84587261114837e-02},
        {0, -2, -1.35830407782663e-06},
        {0, 6, -7.23681885626348e+16},
        {0, 10, -2.23449194054124e+26},
        {1, -12, -1.11526741826431e-35},
        {1, -10, 2.76032601145151e-29},
        {3, 3, 1.34856491567853e+14},
        {4, -6, 6.52440293345860e-10},
        {4, 3, 5.10655119774360e+16},
        {4, 10, -4.68138358908732e+31},
        {5, 2, -7.60667491183279e+15},
        {8, -12, -4.17247986986821e-19},
        {10, -2, 3.12545677756104e+13},
        {12, -3, -1.00375333864186e+14},
        {14, 1, 2.47761392329058e+26},
        };

        static RegionResidualElement Region3Wdata[] = {
        {-12, 8, -5.86219133817016e-08},
        {-12, 14, -8.94460355005526e+10},
        {-10, -1, 5.31168037519774e-31},
        {-10, 8, 1.09892402329239e-01},
        {-8, 6, -5.75368389425212e-02},
        {-8, 8, 2.28276853990249e+04},
        {-8, 14, -1.58548609655002e+18},
        {-6, -4, 3.29865748576503e-28},
        {-6, -3, -6.34987981190669e-25},
        {-6, 2, 6.15762068640611e-09},
        {-6, 8, -9.61109240985747e+07},
        {-5, -10, -4.06274286652625e-45},
        {-4, -1, -4.71103725498077e-13},
        {-4, 3, 7.25937724828145e-01},
        {-3, -10, 1.87768525763682e-39},
        {-3, 3, -1.03308436323771e+03},
        {-2, 1, -6.62552816342168e-02},
        {-2, 2, 5.79514041765710e+02},
        {-1, -8, 2.37416732616644e-27},
        {-1, -4, 2.71700235739893e-15},
        {-1, 1, -9.07886213483600e+01},
        {0, -12, -1.71242509570207e-37},
        {0, 1, 1.56792067854621e+02},
        {1, -1, 9.23261357901470e-01},
        {2, -1, -5.97865988422577},
        {2, 2, 3.21988767636389e+06},
        {3, -12, -3.99441390042203e-30},
        {3, -5, 4.93429086046981e-08},
        {5, -10, 8.12036983370565e-20},
        {5, -8, -2.07610284654137e-12},
        {5, -6, -3.40821291419719e-07},
        {8, -12, 5.42000573372233e-18},
        {8, -10, -8.56711586510214e-13},
        {10, -12, 2.66170454405981e-14},
        {10, -8, 8.58133791857099e-06},
        };

        static RegionResidualElement Region3Xdata[] = {
        {-8, 14, 3.77373741298151e+18},
        {-6, 10, -5.07100883722913e+12},
        {-5, 10, -1.03363225598860e+15},
        {-4, 1, 1.84790814320773e-06},
        {-4, 2, -9.24729378390945e-04},
        {-4, 14, -4.25999562292738e+23},
        {-3, -2, -4.62307771873973e-13},
        {-3, 12, 1.07319065855767e+21},
        {-1, 5, 6.48662492280682e+10},
        {0, 0, 2.44200600688281},
        {0, 4, -8.51535733484258e+09},
        {0, 10, 1.69894481433592e+21},
        {1, -10, 2.15780222509020e-27},
        {1, -1, -3.20850551367334e-01},
        {2, 6, -3.82642448458610e+16},
        {3, -12, -2.75386077674421e-29},
        {3, 0, -5.63199253391666e+05},
        {3, 8, -3.26068646279314e+20},
        {4, 3, 3.97949001553184e+13},
        {5, -6, 1.00824008584757e-07},
        {5, -2, 1.62234569738433e+04},
        {5, 1, -4.32355225319745e+10},
        {6, 1, -5.92874245598610e+11},
        {8, -6, 1.33061647281106},
        {8, -3, 1.57338197797544e+06},
        {8, 1, 2.58189614270853e+13},
        {8, 8, 2.62413209706358e+24},
        {10, -8, -9.20011937431142e-02},
        {12, -10, 2.20213765905426e-03},
        {12, -8, -1.10433759109547e+01},
        {12, -5, 8.47004870612087e+06},
        {12, -4, -5.92910695762536e+08},
        {14, -12, -1.83027173269660e-05},
        {14, -10, 1.81339603516302e-01},
        {14, -8, -1.19228759669889e+03},
        {14, -6, 4.30867658061468e+06},
        };

        static RegionResidualElement Region3Ydata[] = {
        {0, -3, -5.25597995024633e-10},
        {0, 1, 5.83441305228407e+03},
        {0, 5, -1.34778968457925e+16},
        {0, 8, 1.18973500934212e+25},
        {1, 8, -1.59096490904708e+26},
        {2, -4, -3.15839902302021e-07},
        {2, -1, 4.96212197158239e+02},
        {2, 4, 3.27777227273171e+18},
        {2, 5, -5.27114657850696e+21},
        {3, -8, 2.10017506281863e-17},
        {3, 4, 7.05106224399834e+20},
        {3, 8, -2.66713136106469e+30},
        {4, -6, -1.45370512554562e-08},
        {4, 6, 1.49333917053130e+27},
        {5, -2, -1.49795620287641e+07},
        {5, 1, -3.81881906271100e+15},
        {8, -8, 7.24660165585797e-05},
        {8, -2, -9.37808169550193e+13},
        {10, -5, 5.14411468376383e+09},
        {12, -8, -8.28198594040141e+04},
        };

        static RegionResidualElement Region3Zdata[] = {
        {-8, 3, 2.4400789229065e-11},
        {-6, 6, -4.6305743033124e+06},
        {-5, 6, 7.2880327477771e+09},
        {-5, 8, 3.2777630285886e+15},
        {-4, 5, -1.1059817011841e+09},
        {-4, 6, -3.2389991572996e+12},
        {-4, 8, 9.2381400702325e+15},
        {-3, -2, 8.4225008041371e-13},
        {-3, 5, 6.6322143624551e+11},
        {-3, 6, -1.6717018667214e+14},
        {-2, 2, 2.5374935870139e+03},
        {-1, -6, -8.1973155961052e-21},
        {0, 3, 3.2838058789066e+11},
        {1, 1, -6.2500479117154e+07},
        {2, 6, 8.0319795746202e+20},
        {3, -6, -2.0439701133835e-11},
        {3, -2, -3.7839104705594e+03},
        {6, -6, 9.7287654593862e-03},
        {6, -5, 1.5435572168146e+01},
        {6, -4, -3.7396286292864e+03},
        {6, -1, -6.8285901137457e+10},
        {8, -8, -2.4848801561454e-04},
        {8, -4, 3.9453604949707e+06},
        };

        class Region3BackwardsRegion{
        protected:
            double v_star, p_star, T_star, X_star, Y_star;
            std::size_t N;
            double a, b, c, d, e, f;
            std::vector<int> I, J;
            std::vector<double> n;
        public:
    
            Region3BackwardsRegion(RegionResidualElement data[], std::size_t N){
                this->N = N;
                for (std::size_t i = 0; i < N; ++i){
                    n.push_back(data[i].n);
                    I.push_back(data[i].I);
                    J.push_back(data[i].J);
                }
            }
            virtual double v(double T, double p){
                const double pi = p/p_star, theta = T/T_star;
                double summer = 0;
                for (std::size_t i = 0; i < N; ++i){
                    summer += n[i]*pow(pow(pi-a, c), I[i])*pow(pow(theta-b, d), J[i]);
                }
                return pow(summer, e)*v_star;
            };

            // This function piggybacks off of the Backwards structure already written
            // for v(T,p) in Region 3. However, it can be used for the functions T(p,h) [Y=T, X=h] or
            // T(p,s) [Y=T, X=s] in Regions 1, 2, or 3.  Additionally, it can be called for
            // v(p,h) [Y=v, X=h] or v(p,s) [Y=v, X=s] in Region 3
            virtual double Y(double p, double X){
                const double pi = p/p_star, eta = X/X_star;
                double summer = 0;
                for (std::size_t i = 0; i < N; ++i){
                    summer += n[i]*pow(pow(pi-a, c), I[i])*pow(pow(eta-b, d), J[i])*pow(f, J[i]);
                }
                return pow(summer, e)*Y_star;
            };

        };

        class Region3a : public Region3BackwardsRegion{
        public:
            Region3a() : Region3BackwardsRegion(Region3Adata, 30){ 
                v_star = 0.0024; p_star = 100*p_fact; T_star = 760; a = 0.085; b = 0.817; c = 1; d = 1; e = 1;
            };
        };
        class Region3b : public Region3BackwardsRegion{
        public:
            Region3b() : Region3BackwardsRegion(Region3Bdata, 32){
                v_star = 0.0041; p_star = 100*p_fact; T_star = 860; a = 0.280; b = 0.779; c = 1; d = 1; e = 1;
            };
        };
        class Region3c : public Region3BackwardsRegion{
        public:
            Region3c() : Region3BackwardsRegion(Region3Cdata, 35){
                v_star = 0.0022; p_star = 40*p_fact; T_star = 690; a = 0.259; b = 0.903; c = 1; d = 1; e = 1;
            };
        };
        class Region3d : public Region3BackwardsRegion{
        public:
            Region3d() : Region3BackwardsRegion(Region3Ddata, 38){
                v_star = 0.0029; p_star = 40*p_fact; T_star = 690; a = 0.559; b = 0.939; c = 1; d = 1; e = 4;
            };
        };
        class Region3e : public Region3BackwardsRegion{
        public:
            Region3e() : Region3BackwardsRegion(Region3Edata, 29){
                v_star = 0.0032; p_star = 40*p_fact; T_star = 710; a = 0.587; b = 0.918; c = 1; d = 1; e = 1;
            };
        };
        class Region3f : public Region3BackwardsRegion{
        public:
            Region3f() : Region3BackwardsRegion(Region3Fdata, 42){
                v_star = 0.0064; p_star = 40*p_fact; T_star = 730; a = 0.587; b = 0.891; c = 0.5; d = 1; e = 4;
            };
        };
        class Region3g : public Region3BackwardsRegion{
        public:
            Region3g() : Region3BackwardsRegion(Region3Gdata, 38){
                v_star = 0.0027; p_star = 25*p_fact; T_star = 660; a = 0.872; b = 0.971; c = 1; d = 1; e = 4;
            };
        };
        class Region3h : public Region3BackwardsRegion{
        public:
            Region3h() : Region3BackwardsRegion(Region3Hdata, 29){
                v_star = 0.0032; p_star = 25*p_fact; T_star = 660; a = 0.898; b = 0.983; c = 1; d = 1; e = 4;
            };
        };
        class Region3i : public Region3BackwardsRegion{
        public:
            Region3i() : Region3BackwardsRegion(Region3Idata, 42){
                v_star = 0.0041; p_star = 25*p_fact; T_star = 660; a = 0.910; b = 0.984; c = 0.5; d = 1; e = 4;
            };
        };
        class Region3j : public Region3BackwardsRegion{
        public:
            Region3j() : Region3BackwardsRegion(Region3Jdata, 29){
                v_star = 0.0054; p_star = 25*p_fact; T_star = 670; a = 0.875; b = 0.964; c = 0.5; d = 1; e = 4;
            };
        };
        class Region3k : public Region3BackwardsRegion{
        public:
            Region3k() : Region3BackwardsRegion(Region3Kdata, 34){
                v_star = 0.0077; p_star = 25*p_fact; T_star = 680; a = 0.802; b = 0.935; c = 1; d = 1; e = 1;
            };
        };
        class Region3l : public Region3BackwardsRegion{
        public:
            Region3l() : Region3BackwardsRegion(Region3Ldata, 43){
                v_star = 0.0026; p_star = 24*p_fact; T_star = 650; a = 0.908; b = 0.989; c = 1; d = 1; e = 4;
            };
        };
        class Region3m : public Region3BackwardsRegion{
        public:
            Region3m() : Region3BackwardsRegion(Region3Mdata, 40){
                v_star = 0.0028; p_star = 23*p_fact; T_star = 650; a = 1.0; b = 0.997; c = 1; d = 0.25; e = 1;
            };
        };
        class Region3n : public Region3BackwardsRegion{
        public:
            Region3n() : Region3BackwardsRegion(Region3Ndata, 39){
                v_star = 0.0031; p_star = 23*p_fact; T_star = 650; a = 0.976; b = 0.997;
            };
            double v(double T, double p){
                const double pi = p/p_star, theta = T/T_star;
                double summer = 0;
                for (std::size_t i = 0; i < N; ++i){
                    summer += n[i]*pow(pi-a, I[i])*pow(theta-b, J[i]);
                }
                return exp(summer)*v_star;
            };
        };
        class Region3o : public Region3BackwardsRegion{
        public:
            Region3o() : Region3BackwardsRegion(Region3Odata, 24){
                v_star = 0.0034; p_star = 23*p_fact; T_star = 650; a = 0.974; b = 0.996; c = 0.5; d = 1; e = 1;
            };
        };
        class Region3p : public Region3BackwardsRegion{
        public:
            Region3p() : Region3BackwardsRegion(Region3Pdata, 27){
                v_star = 0.0041; p_star = 23*p_fact; T_star = 650; a = 0.972; b = 0.997; c = 0.5; d = 1; e = 1;
            };
        };
        class Region3q : public Region3BackwardsRegion{
        public:
            Region3q() : Region3BackwardsRegion(Region3Qdata, 24){
                v_star = 0.0022; p_star = 23*p_fact; T_star = 650; a = 0.848; b = 0.983; c = 1; d = 1; e = 4;
            };
        };
        class Region3r : public Region3BackwardsRegion{
        public:
            Region3r() : Region3BackwardsRegion(Region3Rdata, 27){
                v_star = 0.0054; p_star = 23*p_fact; T_star = 650; a = 0.874; b = 0.982; c = 1; d = 1; e = 1;
            };
        };
        class Region3s : public Region3BackwardsRegion{
        public:
            Region3s() : Region3BackwardsRegion(Region3Sdata, 29){
                v_star = 0.0022; p_star = 21*p_fact; T_star = 640; a = 0.886; b = 0.990; c = 1; d = 1; e = 4;
            };
        };
        class Region3t : public Region3BackwardsRegion{
        public:
            Region3t() : Region3BackwardsRegion(Region3Tdata, 33){
                v_star = 0.0088; p_star = 20*p_fact; T_star = 650; a = 0.803; b = 1.02; c = 1; d = 1; e = 1;
            };
        };
        class Region3u : public Region3BackwardsRegion{
        public:
            Region3u() : Region3BackwardsRegion(Region3Udata, 38){
                v_star = 0.0026; p_star = 23*p_fact; T_star = 650; a = 0.902; b = 0.988; c = 1; d = 1; e = 1;
            };
        };
        class Region3v : public Region3BackwardsRegion{
        public:
            Region3v() : Region3BackwardsRegion(Region3Vdata, 39){
                v_star = 0.0031; p_star = 23*p_fact; T_star = 650; a = 0.960; b = 0.995; c = 1; d = 1; e = 1;
            };
        };
        class Region3w : public Region3BackwardsRegion{
        public:
            Region3w() : Region3BackwardsRegion(Region3Wdata, 35){
                v_star = 0.0039; p_star = 23*p_fact; T_star = 650; a = 0.959; b = 0.995; c = 1; d = 1; e = 4;
            };
        };
        class Region3x : public Region3BackwardsRegion{
        public:
            Region3x() : Region3BackwardsRegion(Region3Xdata, 36){
                v_star = 0.0049; p_star = 23*p_fact; T_star = 650; a = 0.910; b = 0.988; c = 1; d = 1; e = 1;
            };
        };
        class Region3y : public Region3BackwardsRegion{
        public:
            Region3y() : Region3BackwardsRegion(Region3Ydata, 20){
                v_star = 0.0031; p_star = 22*p_fact; T_star = 650; a = 0.996; b = 0.994; c = 1; d = 1; e = 4;
            };
        };
        class Region3z : public Region3BackwardsRegion{
        public:
            Region3z() : Region3BackwardsRegion(Region3Zdata, 23){
                v_star = 0.0038; p_star = 22*p_fact; T_star = 650; a = 0.993; b = 0.994; c = 1; d = 1; e = 4;
            };
        };

        inline double Region3_v_TP(char region, double T, double p){
            static Region3a R3a;
            static Region3b R3b;
            static Region3c R3c;
            static Region3d R3d;
            static Region3e R3e;
            static Region3f R3f;
            static Region3g R3g;
            static Region3h R3h;
            static Region3i R3i;
            static Region3j R3j;
            static Region3k R3k;
            static Region3l R3l;
            static Region3m R3m;
            static Region3n R3n;
            static Region3o R3o;
            static Region3p R3p;
            static Region3q R3q;
            static Region3r R3r;
            static Region3s R3s;
            static Region3t R3t;
            static Region3u R3u;
            static Region3v R3v;
            static Region3w R3w;
            static Region3x R3x;
            static Region3y R3y;
            static Region3z R3z;

            switch(region){
                case 'A': return R3a.v(T, p);
                case 'B': return R3b.v(T, p);
                case 'C': return R3c.v(T, p);
                case 'D': return R3d.v(T, p);
                case 'E': return R3e.v(T, p);
                case 'F': return R3f.v(T, p);
                case 'G': return R3g.v(T, p);
                case 'H': return R3h.v(T, p);
                case 'I': return R3i.v(T, p);
                case 'J': return R3j.v(T, p);
                case 'K': return R3k.v(T, p);
                case 'L': return R3l.v(T, p);
                case 'M': return R3m.v(T, p);
                case 'N': return R3n.v(T, p);
                case 'O': return R3o.v(T, p);
                case 'P': return R3p.v(T, p);
                case 'Q': return R3q.v(T, p);
                case 'R': return R3r.v(T, p);
                case 'S': return R3s.v(T, p);
                case 'T': return R3t.v(T, p);
                case 'U': return R3u.v(T, p);
                case 'V': return R3v.v(T, p);
                case 'W': return R3w.v(T, p);
                case 'X': return R3x.v(T, p);
                case 'Y': return R3y.v(T, p);
                case 'Z': return R3z.v(T, p);
                default:
                    throw std::out_of_range("Unable to match region");
            }
        }

        struct DivisionElement{
            int I;
            double n;
        };

        static DivisionElement ABdata[] = {
        {0, 0.154793642129415e4},
        {1, -0.187661219490113e3},
        {2, 0.213144632222113e2},
        {-1, -0.191887498864292e4},
        {-2, 0.918419702359447e3},
        };

        static DivisionElement CDdata[] = {
        {0, 0.585276966696349e3},
        {1, 0.278233532206915e1},
        {2, -0.127283549295878e-1},
        {3, 0.159090746562729e-3}
        };

        static DivisionElement GHdata[] = {
        {0, -0.249284240900418e5},
        {1, 0.428143584791546e4},
        {2, -0.269029173140130e3},
        {3, 0.751608051114157e1},
        {4, -0.787105249910383e-1},
        };

        static DivisionElement IJdata[] = {
        {0, 0.584814781649163e3},
        {1, -0.616179320924617},
        {2, 0.260763050899562},
        {3, -0.587071076864459e-2},
        {4, 0.515308185433082e-4},
        };

        static DivisionElement JKdata[] = {
        {0, 0.617229772068439e3},
        {1, -0.770600270141675e1},
        {2, 0.697072596851896},
        {3, -0.157391839848015e-1},
        {4, 0.137897492684194e-3},
        };

        static DivisionElement MNdata[] = {
        {0, 0.535339483742384e3},
        {1, 0.761978122720128e1},
        {2, -0.158365725441648},
        {3, 0.192871054508108e-2},
        };

        static DivisionElement OPdata[] = {
        {0, 0.969461372400213e3},
        {1, -0.332500170441278e3},
        {2, 0.642859598466067e2},
        {-1, 0.773845935768222e3},
        {-2, -0.152313732937084e4},
        };

        static DivisionElement QUdata[] = {
        {0, 0.565603648239126e3},
        {1, 0.529062258221222e1},
        {2, -0.102020639611016},
        {3, 0.122240301070145e-2},
        };

        static DivisionElement RXdata[] = {
        {0, 0.584561202520006e3},
        {1, -0.102961025163669e1},
        {2, 0.243293362700452},
        {3, -0.294905044740799e-2},
        };

        static DivisionElement UVdata[] = {
        {0, 0.528199646263062e3},
        {1, 0.890579602135307e1},
        {2, -0.222814134903755},
        {3, 0.286791682263697e-2},
        };

        static DivisionElement WXdata[] = {
        {0, 0.728052609145380e1},
        {1, 0.973505869861952e2},
        {2, 0.147370491183191e2},
        {-1, 0.329196213998375e3},
        {-2, 0.873371668682417e3},
        };

        class Region3RegionDivision{
        protected:
            std::size_t N;
            std::vector<int> I;
            std::vector<double> n;
        public:
    
            Region3RegionDivision(DivisionElement data[], std::size_t N){
                this->N = N;
                for (std::size_t i = 0; i < N; ++i){
                    n.push_back(data[i].n);
                    I.push_back(data[i].I);
                }
            }
            virtual double T_p(double p){
                const double pi = p/(1.0*p_fact);
                double summer = 0;
                for (std::size_t i = 0; i < N; ++i){
                    summer += n[i]*pow(pi, I[i]);
                }
                return summer*1.0;  // sum is multiplied by T* = 1.0 [K]
            };
        };

        class ABline : public Region3RegionDivision{ 
            public: ABline() : Region3RegionDivision(ABdata, 5){ }; 
            virtual double T_p(double p){
                const double pi = p/(1.0*p_fact), ln_pi = log(pi);
                double summer = 0;
                for (std::size_t i = 0; i < N; ++i){
                    summer += n[i]*pow(ln_pi, I[i]);
                }
                return summer*1.0;  // sum is multiplied by T* = 1.0 [K]
            };
        };
        class CDline : public Region3RegionDivision{ public: CDline() : Region3RegionDivision(CDdata, 4){ }; };
        class EFline { 
        public:
            double T_p(double p){ 
                const double pi = p/(1.0*p_fact); 
                return 3.727888004*(pi - 22.064) + 647.096; 
            }; 
        };
        class GHline : public Region3RegionDivision{ public: GHline() : Region3RegionDivision(GHdata, 5){ }; };
        class IJline : public Region3RegionDivision{ public: IJline() : Region3RegionDivision(IJdata, 5){ }; };
        class JKline : public Region3RegionDivision{ public: JKline() : Region3RegionDivision(JKdata, 5){ }; };
        class MNline : public Region3RegionDivision{ public: MNline() : Region3RegionDivision(MNdata, 4){ }; };
        class OPline : public Region3RegionDivision{ 
        public: 
            OPline() : Region3RegionDivision(OPdata, 5){ }; 
            virtual double T_p(double p){
                const double pi = p/(1.0*p_fact), ln_pi = log(pi);
                double summer = 0;
                for (std::size_t i = 0; i < N; ++i){
                    summer += n[i]*pow(ln_pi, I[i]);
                }
                return summer*1.0;  // sum is multiplied by T* = 1.0 [K]
            };
        };
        class QUline : public Region3RegionDivision{ public: QUline() : Region3RegionDivision(QUdata, 4){ }; };
        class RXline : public Region3RegionDivision{ public: RXline() : Region3RegionDivision(RXdata, 4){ }; };
        class UVline : public Region3RegionDivision{ public: UVline() : Region3RegionDivision(UVdata, 4){ }; };
        class WXline : public Region3RegionDivision{ 
            public: WXline() : Region3RegionDivision(WXdata, 5){ }; 
            virtual double T_p(double p){
                const double pi = p/(1.0*p_fact), ln_pi = log(pi);
                double summer = 0;
                for (std::size_t i = 0; i < N; ++i){
                    summer += n[i]*pow(ln_pi, I[i]);
                }
                return summer*1.0;  // sum is multiplied by T* = 1.0 [K]
            };
        };

        enum DividingLineEnum {LINE_AB, LINE_CD, LINE_EF, LINE_GH, LINE_IJ, LINE_JK, LINE_MN, LINE_OP, LINE_QU, LINE_RX, LINE_UV, LINE_WX};

        inline double DividingLine(DividingLineEnum region, double p){
            static ABline AB;
            static CDline CD;
            static EFline EF;
            static GHline GH;
            static IJline IJ;
            static JKline JK;
            static MNline MN;
            static OPline OP;
            static QUline QU;
            static RXline RX;
            static UVline UV;
            static WXline WX;

            switch(region){
                case LINE_AB: return AB.T_p(p);
                case LINE_CD: return CD.T_p(p);
                case LINE_EF: return EF.T_p(p);
                case LINE_GH: return GH.T_p(p);
                case LINE_IJ: return IJ.T_p(p);
                case LINE_JK: return JK.T_p(p);
                case LINE_MN: return MN.T_p(p);
                case LINE_OP: return OP.T_p(p);
                case LINE_QU: return QU.T_p(p);
                case LINE_RX: return RX.T_p(p);
                case LINE_UV: return UV.T_p(p);
                case LINE_WX: return WX.T_p(p);
                default:
                    throw std::out_of_range("Unable to match dividing line");
            }
        }
        // In the very near critical region, its messy
        inline char BackwardsRegion3SubRegionDetermination(double T, double p){

            if (p > 22.5*p_fact){
               throw std::out_of_range("Out of range");
            }
            else if (22.11*p_fact < p && p <= 22.5*p_fact){
                // Supercritical
                if (DividingLine(LINE_QU, p) < T && T <= DividingLine(LINE_UV, p)){ return 'U';}
                else if (DividingLine(LINE_UV, p) < T && T <= DividingLine(LINE_EF, p)){ return 'V';}
                else if (DividingLine(LINE_EF, p) < T && T <= DividingLine(LINE_WX, p)){ return 'W';}
                else if (DividingLine(LINE_WX, p) < T && T <= DividingLine(LINE_RX, p)){ return 'X';}
                else {return '?';}
            }
            else if (22.064*p_fact < p && p <= 22.11*p_fact){
                // Supercritical
                if (DividingLine(LINE_QU, p) < T && T <= DividingLine(LINE_UV, p)){ return 'U';}
                else if (DividingLine(LINE_UV, p) < T && T <= DividingLine(LINE_EF, p)){ return 'Y';}
                else if (DividingLine(LINE_EF, p) < T && T <= DividingLine(LINE_WX, p)){ return 'Z';}
                else if (DividingLine(LINE_WX, p) < T && T <= DividingLine(LINE_RX, p)){ return 'X';}
                else {return '?';}
            }
            else if (T <= Tsat97(p)){
                if (21.93161551*p_fact < p && p <= 22.064*p_fact){
                    // Sub-critical
                    if (DividingLine(LINE_QU, p) < T && T <= DividingLine(LINE_UV, p)){ return 'U';}
                    else if (DividingLine(LINE_UV, p) < T ){ return 'Y';}
                    else {return '?';}
                }
                else{
                    return 'U';
                }
            }
            else{
                if (21.90096265*p_fact < p && p <= 22.064*p_fact){
                    // Sub-critical
                    if (T <= DividingLine(LINE_WX, p)){ return 'Z';}
                    else if (DividingLine(LINE_WX, p) < T && T <= DividingLine(LINE_RX, p)){ return 'X';}
                    else {return '?';}
                }
                else{
                    return 'X';
                }
            }
        }

        inline char BackwardsRegion3RegionDetermination(double T, double p){
            if (p > 100*p_fact){
                throw std::out_of_range("pressure out of range");
            }
            else if (p > 40*p_fact && p <= 100*p_fact){
                if (T <= DividingLine(LINE_AB, p)){ return 'A';}
                else {return 'B';}
            }
            else if (p > 25*p_fact && p <= 40*p_fact){
                if (T <= DividingLine(LINE_CD, p)){ return 'C';}
                else if (DividingLine(LINE_CD, p) < T && T <= DividingLine(LINE_AB, p)){ return 'D';}
                else if (DividingLine(LINE_AB, p) < T && T <= DividingLine(LINE_EF, p)){ return 'E';}
                else {return 'F';}
            }
            else if (p > 23.5*p_fact && p <= 25*p_fact){
                if (T <= DividingLine(LINE_CD, p)){ return 'C';}
                else if (DividingLine(LINE_CD, p) < T && T <= DividingLine(LINE_GH, p)){ return 'G';}
                else if (DividingLine(LINE_GH, p) < T && T <= DividingLine(LINE_EF, p)){ return 'H';}
                else if (DividingLine(LINE_EF, p) < T && T <= DividingLine(LINE_IJ, p)){ return 'I';}
                else if (DividingLine(LINE_IJ, p) < T && T <= DividingLine(LINE_JK, p)){ return 'J';}
                else {return 'K';}
            }
            else if (p > 23*p_fact && p <= 23.5*p_fact){
                if (T <= DividingLine(LINE_CD, p)){ return 'C';}
                else if (DividingLine(LINE_CD, p) < T && T <= DividingLine(LINE_GH, p)){ return 'L';}
                else if (DividingLine(LINE_GH, p) < T && T <= DividingLine(LINE_EF, p)){ return 'H';}
                else if (DividingLine(LINE_EF, p) < T && T <= DividingLine(LINE_IJ, p)){ return 'I';}
                else if (DividingLine(LINE_IJ, p) < T && T <= DividingLine(LINE_JK, p)){ return 'J';}
                else {return 'K';}
            }
            else if (p > 22.5*p_fact && p <= 23*p_fact){
                if (T <= DividingLine(LINE_CD, p)){ return 'C';}
                else if (DividingLine(LINE_CD, p) < T && T <= DividingLine(LINE_GH, p)){ return 'L';}
                else if (DividingLine(LINE_GH, p) < T && T <= DividingLine(LINE_MN, p)){ return 'M';}
                else if (DividingLine(LINE_MN, p) < T && T <= DividingLine(LINE_EF, p)){ return 'N';}
                else if (DividingLine(LINE_EF, p) < T && T <= DividingLine(LINE_OP, p)){ return 'O';}
                else if (DividingLine(LINE_OP, p) < T && T <= DividingLine(LINE_IJ, p)){ return 'P';}
                else if (DividingLine(LINE_IJ, p) < T && T <= DividingLine(LINE_JK, p)){ return 'J';}
                else {return 'K';}
            }
            else if (p > 21.04336732*p_fact && p <= 22.5*p_fact){
                if (T <= DividingLine(LINE_CD, p)){ return 'C';}
                else if (DividingLine(LINE_CD, p) < T && T <= DividingLine(LINE_QU, p)){ return 'Q';}
                else if (DividingLine(LINE_RX, p) < T && T <= DividingLine(LINE_JK, p)){ return 'R';}
                else if (T > DividingLine(LINE_JK, p)){ return 'K';}
                else{ return BackwardsRegion3SubRegionDetermination(T, p);}
            }
            else if (p > 20.5*p_fact && p <= 21.04336732*p_fact){
                if (T <= DividingLine(LINE_CD, p)){ return 'C';}
                else if (DividingLine(LINE_CD, p) < T && T <= Tsat97(p)){ return 'S';}
                else if (Tsat97(p) < T && T <= DividingLine(LINE_JK, p)){ return 'R';}
                else if (T > DividingLine(LINE_JK, p)){ return 'K';}
                else{ return '?';}
            }
            else if (p > 19.00881189173929*p_fact && p <= 20.5*p_fact){
                if (T <= DividingLine(LINE_CD, p)){ return 'C';}
                else if (DividingLine(LINE_CD, p) < T && T <= Tsat97(p)){ return 'S';}
                else if (Tsat97(p) < T){ return 'T';}
                else{ return '?';}
            }
            else if (p > 16.529164252604481*p_fact && p <= 19.00881189173929*p_fact){
                if (T <= Tsat97(p)){ return 'C';}
                else{ return 'T';}
            }
            else{
                return '?';
            }
        }
    } /* BackwardsRegion3 */

    /********************************************************************************/
    /**************************       Region #3       *******************************/
    /********************************************************************************/
    static RegionResidualElement Region3residdata[] = {
    {0, 0,    0.10658070028513e1},
    {0, 0,   -0.15732845290239e2},
    {0, 1,    0.20944396974307e2},
    {0, 2,   -0.76867707878716e1},
    {0, 7,    0.26185947787954e1},
    {0, 10,  -0.28080781148620e1},
    {0, 12,   0.12053369696517e1},
    {0, 23,  -0.84566812812502e-2},
    {1, 2,   -0.12654315477714e1},
    {1, 6,   -0.11524407806681e1},
    {1, 15,   0.88521043984318},
    {1, 17,  -0.64207765181607},
    {2, 0,    0.38493460186671}, 
    {2, 2,   -0.85214708824206},
    {2, 6,    0.48972281541877e1},
    {2, 7,   -0.30502617256965e1},
    {2, 22,   0.39420536879154e-1},
    {2, 26,   0.12558408424308},
    {3, 0,   -0.27999329698710},
    {3, 2,    0.13899799569460e1},
    {3, 4,   -0.20189915023570e1},
    {3, 16,  -0.82147637173963e-2},
    {3, 26,  -0.47596035734923},
    {4, 0,    0.43984074473500e-1},
    {4, 2,   -0.44476435428739},
    {4, 4,    0.90572070719733},
    {4, 26,   0.70522450087967},
    {5, 1,    0.10770512626332},
    {5, 3,   -0.32913623258954},
    {5, 26,  -0.50871062041158},
    {6, 0,   -0.22175400873096e-1},
    {6, 2,    0.94260751665092e-1},
    {6, 26,   0.16436278447961},
    {7, 2,   -0.13503372241348e-1},
    {8, 26,  -0.14834345352472e-1},
    {9, 2,    0.57922953628084e-3},
    {9, 26,   0.32308904703711e-2},
    {10, 0,   0.80964802996215e-4},
    {10, 1,  -0.16557679795037e-3},
    {11, 26, -0.44923899061815e-4},
    };

    static std::vector<RegionResidualElement> reg3rdata(Region3residdata, Region3residdata + sizeof(Region3residdata)/sizeof(RegionResidualElement));

    class Region3
    {
    protected:
        std::vector<int> Ir, Jr;
        std::vector<double> nr;
        /// For Viscosity Calculations
        std::vector<int> muJ0;
        std::vector<double> mun0;
        std::vector<int> muIr, muJr;
        std::vector<double> munr;
        /// For Thermal Conductivity Calculations
        std::vector<int> lamJ0;
        std::vector<double> lamn0;
        std::vector<int> lamIr, lamJr;
        std::vector<double> lamnr;
        double T_star, p_star, R;
    public:
        Region3() : T_star(1000), p_star(1*p_fact) {
            for (std::size_t i = 0; i < reg3rdata.size(); ++i){
                nr.push_back(reg3rdata[i].n);
                Ir.push_back(reg3rdata[i].I);
                Jr.push_back(reg3rdata[i].J);
            }
            for (std::size_t i = 0; i < Hrdata.size(); ++i){
                munr.push_back(Hrdata[i].n);
                muIr.push_back(Hrdata[i].I);
                muJr.push_back(Hrdata[i].J);
            }
            for (std::size_t i = 0; i < H0data.size(); ++i){
                mun0.push_back(H0data[i].n);
                muJ0.push_back(H0data[i].J);
            }
            for (std::size_t i = 0; i < Lrdata.size(); ++i){
                lamnr.push_back(Lrdata[i].n);
                lamIr.push_back(Lrdata[i].I);
                lamJr.push_back(Lrdata[i].J);
            }
            for (std::size_t i = 0; i < L0data.size(); ++i){
                lamn0.push_back(L0data[i].n);
                lamJ0.push_back(L0data[i].J);
            }
            R = Rgas;
        };
        double phi(double T, double rho) const{
            const double delta = rho/Rhocrit, tau = Tcrit/T;
            double summer = nr[0]*log(delta);
            for (std::size_t i = 1; i < 40; ++i){
                summer += nr[i]*pow(delta, Ir[i])*pow(tau, Jr[i]);
            }
            return summer;
        };
#ifdef REGION3_ITERATE
        //
        // These two extra terms Needed to evaluate Newton-Raphson
        // ****************************************************************************
        double dphi_ddelta(double T, double rho) const{
            const double delta = rho/Rhocrit, tau = Tcrit/T;
            double summer = nr[0]/delta;
            for (std::size_t i = 1; i < 40; ++i){
                summer += nr[i]*Ir[i]*pow(delta, Ir[i]-1)*pow(tau, Jr[i]);
            }
            return summer;
        };
        double d2phi_ddelta2(double T, double rho) const{
            const double delta = rho/Rhocrit, tau = Tcrit/T;
            double summer = -nr[0]/(delta*delta);
            for (std::size_t i = 1; i < 40; ++i){
                summer += nr[i]*Ir[i]*(Ir[i]-1.0)*pow(delta, Ir[i]-2)*pow(tau, Jr[i]);
            }
            return summer;
        };
        // ****************************************************************************
#endif
        double delta_dphi_ddelta(double T, double rho) const{
            const double delta = rho/Rhocrit, tau = Tcrit/T;
            double summer = nr[0];
            for (std::size_t i = 1; i < 40; ++i){
                summer += nr[i]*Ir[i]*pow(delta, Ir[i])*pow(tau, Jr[i]);
            }
            return summer;
        };
        double tau_dphi_dtau(double T, double rho) const{
            const double delta = rho/Rhocrit, tau = Tcrit/T;
            double summer = 0;
            for (std::size_t i = 1; i < 40; ++i){
                summer += nr[i]*Jr[i]*pow(delta, Ir[i])*pow(tau, Jr[i]);
            }
            return summer;
        };
        double delta2_d2phi_ddelta2(double T, double rho) const{
            const double delta = rho/Rhocrit, tau = Tcrit/T;
            double summer = -nr[0];
            for (std::size_t i = 1; i < 40; ++i){
                summer += nr[i]*Ir[i]*(Ir[i]-1)*pow(delta, Ir[i])*pow(tau, Jr[i]);
            }
            return summer;
        };
        double tau2_d2phi_dtau2(double T, double rho) const{
            const double delta = rho/Rhocrit, tau = Tcrit/T;
            double summer = 0;
            for (std::size_t i = 1; i < 40; ++i){
                summer += nr[i]*Jr[i]*(Jr[i]-1)*pow(delta, Ir[i])*pow(tau, Jr[i]);
            }
            return summer;
        };
        double deltatau_d2phi_ddelta_dtau(double T, double rho) const{
            const double delta = rho/Rhocrit, tau = Tcrit/T;
            double summer = 0;
            for (std::size_t i = 1; i < 40; ++i){
                summer += nr[i]*Jr[i]*Ir[i]*pow(delta, Ir[i])*pow(tau, Jr[i]);
            }
            return summer;
        };
        double mu0(double T) const{
            const double T_bar = T/Tcrit;
            double summer = 0.0;
            for (std::size_t i = 0; i < muJ0.size(); ++i)
            {
                summer += mun0[i]/pow(T_bar, muJ0[i]);
            }
            return 100.0*sqrt(T_bar)/summer;
        }
        double mu1(double T, double rho) const{
            const double rho_bar = rho/Rhocrit;
            double summer = 0.0;
            for (std::size_t i = 0; i < muJr.size(); ++i){
                summer += rho_bar * pow(Trterm(T),muIr[i]) * munr[i]*pow(Rhorterm(rho),muJr[i]);
            }
            return exp(summer);
        }
        double lambda0(double T) const{
            const double T_bar = T/Tcrit;
            double summer = 0.0;
            for (std::size_t i = 0; i < lamJ0.size(); ++i){
                summer += lamn0[i]/pow(T_bar, lamJ0[i]);
            }
            return sqrt(T_bar)/summer;
        }
        double lambda1(double T, double rho) const{
            const double rho_bar = rho/Rhocrit;
            double summer = 0.0;
            for (std::size_t i = 0; i < lamJr.size(); ++i){
                summer += rho_bar * pow(Trterm(T),lamIr[i]) * lamnr[i]*pow(Rhorterm(rho),lamJr[i]);
            }
            return exp(summer);
        }
        double lambda2(double T, double /*p*/, double rho) const{
            double y, Cpbar, mubar, k, Z, zeta, delChi, Cpcalc;
            const double rhobar = rho/Rhocrit;   /// Dimensionless
            const double LAMBDA = 177.8514;      /// Dimensionless
            const double qD     = 1.0/0.40;      /// 1/nm
            const double Tr     = 1.5*Tcrit;     /// Dimensionless
            const double xi0    = 0.13;          /// nm
            const double nu     = 0.630;         /// Dimensionless
            const double gam    = 1.239;         /// Dimensionless
            const double GAMMA0 = 0.06;          /// Dimensionless
            const double PI     = 2*acos(0.0);   /// Have to define this in C++
            const double Cpstar = 0.46151805*R_fact;  /// Note: Slightly lower than IF97 Rgas  {J/kg-K}
            Cpcalc = cpmass(T,rho);                                  /// J/kg-K
            Cpbar = Cpcalc/Cpstar;                                   /// Unit-less
            if ((Cpbar < 0) || (Cpbar > 1.0E13)) Cpbar = 1.0E13;     /// Unit-less
            k = Cpcalc/cvmass(T,rho);                                /// Unit-less
            mubar = visc(T,rho)/1.0E-6;                              /// Unit-less
            zeta = Pcrit/Rhocrit*drhodp(T,rho);                      /// 
            if ((zeta < 0) || (zeta > 1.0E13)) zeta = 1.0E13;
            delChi = rhobar*(zeta - delTr(rho)*Tr/T);
            y = qD*xi0*pow(delChi/GAMMA0,nu/gam);
            if (y < 1.2E-7) 
                Z = 0.0;
            else
                Z = 2.0/(PI*y)*(((1.0-1.0/k)*atan(y)+y/k) - (1.0 - exp(-1.0/(1.0/y + y*y/(3.0*rhobar*rhobar)))));
            return LAMBDA*rhobar*Cpbar*T/(Tcrit*mubar)*Z;
        }
        double Trterm(double T) const{
            return Tcrit/T - 1.0;
        }
        double Rhorterm(double rho) const{
            return rho/Rhocrit - 1.0;
        }
        double p(double T, double rho) const{
            return rho*R*T*delta_dphi_ddelta(T, rho)*(p_fact/1000/R_fact);
        };

#ifdef REGION3_ITERATE
        //
        // Newton-Raphson Technique for solving p(T,rho) for rho
        //    Solves to find root of p - rho*R*T*delta*dphi_ddelta = 0
        //    The equation is rearranged to solve for rho and turned
        //    into functions f(T,P,rho0) and f'(T,P,rho0) for the
        //    Newton-Raphson technique.  Functions for
        //    dphi/ddelta and d²phi/ddelta² were also required.  These
        //    additional Taylor functions are defined above.
        //
        double f(double T, double p, double rho0) const{
            return 1.0/(rho0*rho0) - R*T*dphi_ddelta(T, rho0)/(p*Rhocrit)*(p_fact/1000/R_fact);
        };
        double df(double T, double p, double rho0) const{
            const double rho_c = 322.0, rho_c2 = rho_c*rho_c;
            return -2.0/(rho0*rho0*rho0) - R*T*d2phi_ddelta2(T, rho0)/(p*rho_c2)*(p_fact/1000/R_fact);
        };
        double rhomass(double T, double p, double rho0) const {
            int iter = 100;
            double f_T_p_rho0 = f(T,p,rho0);
            while ( std::abs(f_T_p_rho0) > 1.0e-14 )
            {
                rho0 -= ( f_T_p_rho0/df(T,p,rho0) );
                // don't go more than 100 iterations or throw an exception
                if (--iter == 0) throw std::logic_error("Failed to converge!"); 
                f_T_p_rho0 = f(T,p,rho0);
            }
            return rho0;
        }
        // END Newton-Raphson
#endif

        double umass(double T, double rho) const{
            return R*T*tau_dphi_dtau(T, rho);
        };
        double smass(double T, double rho) const{
            return R*(tau_dphi_dtau(T, rho) - phi(T, rho));
        };
        double hmass(double T, double rho) const{
            return R*T*(tau_dphi_dtau(T, rho) + delta_dphi_ddelta(T, rho));
        };
        double cpmass(double T, double rho) const{
            return R*(-tau2_d2phi_dtau2(T, rho) + pow(delta_dphi_ddelta(T, rho) - deltatau_d2phi_ddelta_dtau(T, rho), 2)/(2*delta_dphi_ddelta(T, rho) + delta2_d2phi_ddelta2(T, rho)));
        };
        double cvmass(double T, double rho) const{
            return R*(-tau2_d2phi_dtau2(T, rho));
        };
        double speed_sound(double T, double rho) const{
            const double RHS = 2*delta_dphi_ddelta(T, rho) + delta2_d2phi_ddelta2(T, rho)-pow(delta_dphi_ddelta(T,rho)-deltatau_d2phi_ddelta_dtau(T,rho),2)/tau2_d2phi_dtau2(T,rho);
            return sqrt(R*(1000/R_fact)*T*RHS);
        }
        double visc(double T, double rho) const{
            /// This base region function was not inherited 
            const double mu_star = 1.0E-6; // Reference viscosity [Pa-s]
            const double mu2 = 1.0;        // For Industrial Formulation (IF97), mu2 = 1.0
            return mu_star * mu0(T) * mu1(T,rho) * mu2;
        }
        double tcond(double T, double p, double rho) const{
            /// This base region function was not inherited in Region3
            const double lambda_star = 0.001;
            const double lambda_bar = lambda0(T)*lambda1(T,rho) + lambda2(T,p,rho);
            return lambda_star * lambda_bar;
        }
        double drhodp(double T, double rho) const
        /// Derived from IAPWS Revised Advisory Note No. 3 (See Table 2, Section 3.1 & 3.3)
        /// NOTE: rho is passed in here, not p as it is in Regions 1, 2, & 5.  This is done
        ///       because p(T,rho) is a simple algebraic in Region 3, the Helmholz functions
        ///       are functions of (T,rho), and the work has already been done by the output()
        ///       function to convert p to rho @ T.
        {
            return (rho/p(T,rho)) / ( 2.0 + delta2_d2phi_ddelta2(T,rho)/delta_dphi_ddelta(T,rho) );
        }
        double delTr(double rho) const{
            /// This is the IF97 correlation for drhodp at the reducing temperature, Tr
            const double rhobar = rho/Rhocrit;
            double summer = 0;
            int j;
            //
            if      (rhobar <= 0.310559006) j = 0;
            else if (rhobar <= 0.776397516) j = 1;
            else if (rhobar <= 1.242236025) j = 2;
            else if (rhobar <= 1.863354037) j = 3;
            else                            j = 4;
            //
            double pow_rhobar = 1.0;
            for (int i=0; i < 6; ++i){
                summer += A[i][j]*pow_rhobar;
                pow_rhobar *= rhobar;
            }
            return 1.0/summer;
        }
        char SatSubRegionAdjust(IF97SatState State, double p, char subregion) const{
            switch(State)      // See if saturated state is requested
            {
                                  // If looking for Saturated Vapor...
                case VAPOR: {     // ...force below saturation curve
                                if (subregion == 'C') return 'T';
                                else if (subregion == 'S')
                                {
                                    if ( p < 20.5*p_fact ) 
                                        return 'T';
                                    else 
                                        return 'R';
                                }
                                else if (subregion == 'U')
                                {
                                    if ( p < 21.90096265*p_fact ) 
                                        return 'X';
                                    else 
                                        return 'Z';
                                }
                                else if (subregion == 'Y') return 'Z';
                                break;
                            };

                                  // If looking for Saturated Liquid...
                case LIQUID:{     // ...force above saturation curve
                                if (subregion == 'Z') {
                                    if ( p > 21.93161551*p_fact )
                                        return 'Y';
                                    else 
                                        return 'U'; 
                                }
                                else if (subregion == 'X') return 'U';
                                else if ((subregion == 'R') || (subregion == 'K')) return 'S';
                                else if (subregion == 'T'){
                                    if ( p > 19.00881189173929*p_fact )
                                        return 'S';
                                    else 
                                        return 'C';
                                }
                                break;
                            };
                case NONE:
                default: return subregion;
            }
            return subregion;  // in case no adjustment needs to be made
        };  // SatSubRegionAdjust

        double output(IF97parameters key, double T, double p, IF97SatState State){
            double rho;
            char region = Region3Backwards::BackwardsRegion3RegionDetermination(T, p);

            // if this is a saturated vapor or liquid function, make sure we're on
            // the correct side of the saturation curve and adjust region before
            // calculating density.
            region = SatSubRegionAdjust(State, p, region);

            rho = 1/Region3Backwards::Region3_v_TP(region, T, p);

#ifdef REGION3_ITERATE
            // Use previous rho value from algebraic equations 
            //      as an initial guess to solve rhomass iteratively 
            //      with Newton-Raphson
            rho = rhomass(T, p, rho);   
#endif
            switch(key)                 // return all properties using the new rho value
            {
                case IF97_DMASS: return rho;
                case IF97_HMASS: return hmass(T, rho);
                case IF97_SMASS: return smass(T, rho);
                case IF97_UMASS: return umass(T, rho);
                case IF97_CPMASS: return cpmass(T, rho);
                case IF97_CVMASS: return cvmass(T, rho);
                case IF97_W: return speed_sound(T, rho);
                case IF97_MU: return visc(T,rho);
                case IF97_K: return tcond(T,p,rho);
                case IF97_DRHODP: return drhodp(T, rho); 

                default:
                    throw std::invalid_argument("Bad key to output");  // JPH: changed this to invalid_argument exception
            }
        }
    };

    /********************************************************************************/
    /**************************       Region #4       *******************************/
    /********************************************************************************/
    struct SaturationElement{
        int i;
        double n;
    };
    static SaturationElement sat[] = {
        {1,  0.11670521452767e4},
        {2, -0.72421316703206e6},
        {3, -0.17073846940092e2},
        {4,  0.12020824702470e5},
        {5, -0.32325550322333e7},
        {6,  0.14915108613530e2},
        {7, -0.48232657361591e4},
        {8,  0.40511340542057e6},
        {9, -0.23855557567849},
        {10, 0.65017534844798e3},
    };
    static std::vector<SaturationElement> reg4data(sat, sat + sizeof(sat)/sizeof(SaturationElement));
    /// This "region" is the saturation curve
    class Region4
    {
    public:
        std::vector<double> n;
        double p_star, T_star;

        Region4() : p_star(1.0*p_fact), T_star(1.0) {
            n.resize(1); n[0] = 0;
            for (std::size_t i = 0; i < reg4data.size(); ++i){
                n.push_back(reg4data[i].n);
            }
        };
        double p_T(double T) const{
            // Allow extrapolation down to Pmin = P(Tmin=273.15K) = 611.213 Pa
            if ( ( T < Tmin ) || ( T > Tcrit ) ){
                throw std::out_of_range("Temperature out of range");
            }
            const double theta = T/T_star+n[9]/(T/T_star-n[10]);
            const double A =      theta*theta + n[1]*theta + n[2];
            const double B = n[3]*theta*theta + n[4]*theta + n[5];
            const double C = n[6]*theta*theta + n[7]*theta + n[8];
            return p_star*pow(2*C/(-B+sqrt(B*B-4*A*C)), 4);
        };
        double T_p(double p) const{
            // Allow extrapolation down to Pmin = P(Tmin=273.15K) = 611.213 Pa
            if ( ( p < Pmin ) || ( p > Pcrit ) ){
                throw std::out_of_range("Pressure out of range");
            }
            

            /* // Initial formulas
            const double beta = std::pow(beta, 0.25);
            const double E =      beta*beta + n[3]*beta + n[6];
            const double F = n[1]*beta*beta + n[4]*beta + n[7];
            const double G = n[2]*beta*beta + n[5]*beta + n[8];
            */

            const double beta2 = std::sqrt(p/p_star);
            const double beta  = std::sqrt(beta2);
            /* // First optimization
            const double E =      beta2 + n[3]*beta + n[6];
            const double F = n[1]*beta2 + n[4]*beta + n[7];
            const double G = n[2]*beta2 + n[5]*beta + n[8];
            */

            static double EFG[3];
            static double &E = EFG[0];
            static double &F = EFG[1];
            static double &G = EFG[2];

            // Each cycle can be vectorized
            EFG[0] = 1.0; EFG[1] = n[1]; EFG[2] = n[2];
            for(int i=0; i<3; ++i){
                EFG[i] *= beta;
            }

            for(int i=0; i<3; ++i){
                EFG[i] += n[i+3];
            }

            for(int i=0; i<3; ++i){
                EFG[i] *= beta;
            }

            for(int i=0; i<3; ++i){
                EFG[i] += n[i+6];
            }

            /* // Each row can be vectorized
            //EFG[0]  = beta2;     EFG[1]  = n[1]*beta2; EFG[2]  = n[2]*beta2;
            //EFG[0] += n[3]*beta; EFG[1] += n[4]*beta;  EFG[2] += n[5]*beta;
            //EFG[0] += n[6];      EFG[1] += n[7];       EFG[2] += n[8];
            */

            const double D = 2*G/(-F-std::sqrt(F*F-4*E*G));
            const double n10pD = n[10]+D;
            return T_star*0.5*(n10pD - std::sqrt(n10pD*n10pD - 4*(n[9] + n[10]*D)));
        };
		double sigma_t(double T) const{
            // Surface Tension [mN/m] in two-phase region as a function of temperature [K]
            // Implemented from IAPWS R1-76(2014).
            // May be extrapolated down to -25C in the super-cooled region.
			if ( ( T < (Ttrip - 25.0) ) || ( T > Tcrit ) ){
                throw std::out_of_range("Temperature out of range");
            }
			const double Tau = 1.0 - T/Tcrit;
			const double B = 235.8 / 1000;  // Published value in [mN/m]; Convert to SI [N/m] in all cases 
			const double b = -0.625;
			const double mu = 1.256;
			return B*pow(Tau,mu)*(1.0 + b*Tau);
		}
    };

    /********************************************************************************/
    /**************************       Region #5       *******************************/
    /********************************************************************************/
    static RegionResidualElement Region5residdata[] = {
        {1, 1,  0.15736404855259e-2},
        {1, 2,  0.90153761673944e-3},
        {1, 3, -0.50270077677648e-2},
        {2, 3,  0.22440037409485e-5},
        {2, 9, -0.41163275453471e-5},
        {3, 7,  0.37919454822955e-7}
    };
    static RegionIdealElement Region5idealdata[] = {
        { 0, -0.13179983674201e2},
        { 1,  0.68540841634434e1},
        {-3, -0.24805148933466e-1},
        {-2,  0.36901534980333},
        {-1, -0.31161318213925e1},
        { 2, -0.32961626538917}
    };
    static std::vector<RegionResidualElement> reg5rdata(Region5residdata, Region5residdata + sizeof(Region5residdata)/sizeof(RegionResidualElement));
    static std::vector<RegionIdealElement> reg50data(Region5idealdata, Region5idealdata + sizeof(Region5idealdata)/sizeof(RegionIdealElement));

    class Region5 : public BaseRegion
    {
    public:
        Region5() : BaseRegion(reg5rdata, reg50data)  {
            T_star = 1000; p_star = 1*p_fact; 
        };
        double lambda2(double /*T*/, double /*p*/, double /*rho*/) const{
            return 0.0;  // No critical enhancement of thermal conductivity in Region 5
        }
        double TAUrterm(double T) const{
            return T_star/T;
        }
        double PIrterm(double p) const{
            return p/p_star;
        }
        double TAU0term(double T) const{
            return T_star/T;
        }
    };

    /********************************************************************************/
    /*********************         Backwards Regions        *************************/
    /********** Implementation for T(p,h), v(p,h), T(p,s), and v(p,s)  **************/
    /********************************************************************************/

    namespace Backwards{

        struct BackwardRegionResidualElement
        {
            double I, ///< The first index
                   J; ///< The second index
            double n; ///< The leading numerical constant
        };

        static BackwardRegionResidualElement Coeff1H[] = {
            {0,  0, -0.23872489924521E+03},
            {0,  1,  0.40421188637945E+03},
            {0,  2,  0.11349746881718E+03},
            {0,  6, -0.58457616048039E+01},
            {0, 22, -0.15285482413140E-03},
            {0, 32, -0.10866707695377E-05},
            {1,  0, -0.13391744872602E+02},
            {1,  1,  0.43211039183559E+02},
            {1,  2, -0.54010067170506E+02},
            {1,  3,  0.30535892203916E+02},
            {1,  4, -0.65964749423638E+01},
            {1, 10,  0.93965400878363E-02},
            {1, 32,  0.11573647505340E-06},
            {2, 10, -0.25858641282073E-04},
            {2, 32, -0.40644363084799E-08},
            {3, 10,  0.66456186191635E-07},
            {3, 32,  0.80670734103027E-10},
            {4, 32, -0.93477771213947E-12},
            {5, 32,  0.58265442020601E-14},
            {6, 32, -0.15020185953503E-16}
        };

        static BackwardRegionResidualElement Coeff1S[] = {
            {0,  0,  0.17478268058307E+03},
            {0,  1,  0.34806930892873E+02},
            {0,  2,  0.65292584978455E+01},
            {0,  3,  0.33039981775489E+00},
            {0, 11, -0.19281382923196E-06},
            {0, 31, -0.24909197244573E-22},
            {1,  0, -0.26107636489332E+00},
            {1,  1,  0.22592965981586E+00},
            {1,  2, -0.64256463395226E-01},
            {1,  3,  0.78876289270526E-02},
            {1, 12,  0.35672110607366E-09},
            {1, 31,  0.17332496994895E-23},
            {2,  0,  0.56608900654837E-03},
            {2,  1, -0.32635483139717E-03},
            {2,  2,  0.44778286690632E-04},
            {2,  9, -0.51322156908507E-09},
            {2, 31, -0.42522657042207E-25},
            {3, 10,  0.26400441360689E-12},
            {3, 32,  0.78124600459723E-28},
            {4, 32, -0.30732199903668E-30}
        };

        static BackwardRegionResidualElement Coeff1HS[] = {
            {0,  0, -0.691997014660582E0},
            {0,  1, -0.183612548787560E2},
            {0,  2, -0.928332409297335E1},
            {0,  4,  0.659639569909906E2},
            {0,  5, -0.162060388912024E2},
            {0,  6,  0.450620017338667E3},
            {0,  8,  0.854680678224170E3},
            {0, 14,  0.607523214001162E4},
            {1,  0,  0.326487682621856E2},
            {1,  1, -0.269408844582931E2},
            {1,  4, -0.319947848334300E3},
            {1,  6, -0.928354307043320E3},
            {2,  0,  0.303634537455249E2},
            {2,  1, -0.650540422444146E2},
            {2, 10, -0.430991316516130E4},
            {3,  4, -0.747512324096068E3},
            {4,  1,  0.730000345529245E3},
            {4,  4,  0.114284032569021E4},
            {5,  0, -0.436407041874559E3}
        };

        static BackwardRegionResidualElement Coeff2aH[] = {
            {0,  0,  0.10898952318288E+04},
            {0,  1,  0.84951654495535E+03},
            {0,  2, -0.10781748091826E+03},
            {0,  3,  0.33153654801263E+02},
            {0,  7, -0.74232016790248E+01},
            {0, 20,  0.11765048724356E+02},
            {1,  0,  0.18445749355790E+01},
            {1,  1, -0.41792700549624E+01},
            {1,  2,  0.62478196935812E+01},
            {1,  3, -0.17344563108114E+02},
            {1,  7, -0.20058176862096E+03},
            {1,  9,  0.27196065473796E+03},
            {1, 11, -0.45511318285818E+03},
            {1, 18,  0.30919688604755E+04},
            {1, 44,  0.25226640357872E+06},
            {2,  0, -0.61707422868339E-02},
            {2,  2, -0.31078046629583E+00},
            {2,  7,  0.11670873077107E+02},
            {2, 36,  0.12812798404046E+09},
            {2, 38, -0.98554909623276E+09},
            {2, 40,  0.28224546973002E+10},
            {2, 42, -0.35948971410703E+10},
            {2, 44,  0.17227349913197E+10},
            {3, 24, -0.13551334240775E+05},
            {3, 44,  0.12848734664650E+08},
            {4, 12,  0.13865724283226E+01},
            {4, 32,  0.23598832556514E+06},
            {4, 44, -0.13105236545054E+08},
            {5, 32,  0.73999835474766E+04},
            {5, 36, -0.55196697030060E+06},
            {5, 42,  0.37154085996233E+07},
            {6, 34,  0.19127729239660E+05},
            {6, 44, -0.41535164835634E+06},
            {7, 28, -0.62459855192507E+02}
        };

        static BackwardRegionResidualElement Coeff2bH[] = {
            {0,  0,  0.14895041079516E+04},
            {0,  1,  0.74307798314034E+03},
            {0,  2, -0.97708318797837E+02},
            {0, 12,  0.24742464705674E+01},
            {0, 18, -0.63281320016026E+00},
            {0, 24,  0.11385952129658E+01},
            {0, 28, -0.47811863648625E+00},
            {0, 40,  0.85208123431544E-02},
            {1,  0,  0.93747147377932E+00},
            {1,  2,  0.33593118604916E+01},
            {1,  6,  0.33809355601454E+01},
            {1, 12,  0.16844539671904E+00},
            {1, 18,  0.73875745236695E+00},
            {1, 24, -0.47128737436186E+00},
            {1, 28,  0.15020273139707E+00},
            {1, 40, -0.21764114219750E-02},
            {2,  2, -0.21810755324761E-01},
            {2,  8, -0.10829784403677E+00},
            {2, 18, -0.46333324635812E-01},
            {2, 40,  0.71280351959551E-04},
            {3,  1,  0.11032831789999E-03},
            {3,  2,  0.18955248387902E-03},
            {3, 12,  0.30891541160537E-02},
            {3, 24,  0.13555504554949E-02},
            {4,  2,  0.28640237477456E-06},
            {4, 12, -0.10779857357512E-04},
            {4, 18, -0.76462712454814E-04},
            {4, 24,  0.14052392818316E-04},
            {4, 28, -0.31083814331434E-04},
            {4, 40, -0.10302738212103E-05},
            {5, 18,  0.28217281635040E-06},
            {5, 24,  0.12704902271945E-05},
            {5, 40,  0.73803353468292E-07},
            {6, 28, -0.11030139238909E-07},
            {7,  2, -0.81456365207833E-13},
            {7, 28, -0.25180545682962E-10},
            {9,  1, -0.17565233969407E-17},
            {9, 40,  0.86934156344163E-14}
        };

        static BackwardRegionResidualElement Coeff2cH[] = {
            {-7,  0, -0.32368398555242E+13},
            {-7,  4,  0.73263350902181E+13},
            {-6,  0,  0.35825089945447E+12},
            {-6,  2, -0.58340131851590E+12},
            {-5,  0, -0.10783068217470E+11},
            {-5,  2,  0.20825544563171E+11},
            {-2,  0,  0.61074783564516E+06},
            {-2,  1,  0.85977722535580E+06},
            {-1,  0, -0.25745723604170E+05},
            {-1,  2,  0.31081088422714E+05},
            { 0,  0,  0.12082315865936E+04},
            { 0,  1,  0.48219755109255E+03},
            { 1,  4,  0.37966001272486E+01},
            { 1,  8, -0.10842984880077E+02},
            { 2,  4, -0.45364172676660E-01},
            { 6,  0,  0.14559115658698E-12},
            { 6,  1,  0.11261597407230E-11},
            { 6,  4, -0.17804982240686E-10},
            { 6, 10,  0.12324579690832E-06},
            { 6, 12, -0.11606921130984E-05},
            { 6, 16,  0.27846367088554E-04},
            { 6, 20, -0.59270038474176E-03},
            { 6, 22,  0.12918582991878E-02}
        };

        static BackwardRegionResidualElement Coeff2aS[] = {
            {-1.50, -24, -0.39235983861984E+6},
            {-1.50, -23,  0.51526573827270E+6},
            {-1.50, -19,  0.40482443161048E+5},
            {-1.50, -13, -0.32193790923902E+3},
            {-1.50, -11,  0.96961424218694E+2},
            {-1.50, -10, -0.22867846371773E+2},
            {-1.25, -19, -0.44942914124357E+6},
            {-1.25, -15, -0.50118336020166E+4},
            {-1.25,  -6,  0.35684463560015E+0},
            {-1.00, -26,  0.44235335848190E+5},
            {-1.00, -21, -0.13673388811708E+5},
            {-1.00, -17,  0.42163260207864E+6},
            {-1.00, -16,  0.22516925837475E+5},
            {-1.00,  -9,  0.47442144865646E+3},
            {-1.00,  -8, -0.14931130797647E+3},
            {-0.75, -15, -0.19781126320452E+6},
            {-0.75, -14, -0.23554399470760E+5},
            {-0.50, -26, -0.19070616302076E+5},
            {-0.50, -13,  0.55375669883164E+5},
            {-0.50,  -9,  0.38293691437363E+4},
            {-0.50,  -7, -0.60391860580567E+3},
            {-0.25, -27,  0.19363102620331E+4},
            {-0.25, -25,  0.42660643698610E+4},
            {-0.25, -11, -0.59780638872718E+4},
            {-0.25,  -6, -0.70401463926862E+3},
            { 0.25,   1,  0.33836784107553E+3},
            { 0.25,   4,  0.20862786635187E+2},
            { 0.25,   8,  0.33834172656196E-1},
            { 0.25,  11, -0.43124428414893E-4},
            { 0.50,   0,  0.16653791356412E+3},
            { 0.50,   1, -0.13986292055898E+3},
            { 0.50,   5, -0.78849547999872E+0},
            { 0.50,   6,  0.72132411753872E-1},
            { 0.50,  10, -0.59754839398283E-2},
            { 0.50,  14, -0.12141358953904E-4},
            { 0.50,  16,  0.23227096733871E-6},
            { 0.75,   0, -0.10538463566194E+2},
            { 0.75,   4,  0.20718925496502E+1},
            { 0.75,   9, -0.72193155260427E-1},
            { 0.75,  17,  0.20749887081120E-6},
            { 1.00,   7, -0.18340657911379E-1},
            { 1.00,  18,  0.29036272348696E-6},
            { 1.25,   3,  0.21037527893619E+0},
            { 1.25,  15,  0.25681239729999E-3},
            { 1.50,   5, -0.12799002933781E-1},
            { 1.50,  18, -0.82198102652018E-5}
        };

        static BackwardRegionResidualElement Coeff2bS[] = {
            {-6,  0,  0.31687665083497E+6},
            {-6, 11,  0.20864175881858E+2},
            {-5,  0, -0.39859399803599E+6},
            {-5, 11, -0.21816058518877E+2},
            {-4,  0,  0.22369785194242E+6},
            {-4,  1, -0.27841703445817E+4},
            {-4, 11,  0.99207436071480E+1},
            {-3,  0, -0.75197512299157E+5},
            {-3,  1,  0.29708605951158E+4},
            {-3, 11, -0.34406878548526E+1},
            {-3, 12,  0.38815564249115E+0},
            {-2,  0,  0.17511295085750E+5},
            {-2,  1, -0.14237112854449E+4},
            {-2,  6,  0.10943803364167E+1},
            {-2, 10,  0.89971619308495E+0},
            {-1,  0, -0.33759740098958E+4},
            {-1,  1,  0.47162885818355E+3},
            {-1,  5, -0.19188241993679E+1},
            {-1,  8,  0.41078580492196E+0},
            {-1,  9, -0.33465378172097E+0},
            { 0,  0,  0.13870034777505E+4},
            { 0,  1, -0.40663326195838E+3},
            { 0,  2,  0.41727347159610E+2},
            { 0,  4,  0.21932549434532E+1},
            { 0,  5, -0.10320050009077E+1},
            { 0,  6,  0.35882943516703E+0},
            { 0,  9,  0.52511453726066E-2},
            { 1,  0,  0.12838916450705E+2},
            { 1,  1, -0.28642437219381E+1},
            { 1,  2,  0.56912683664855E+0},
            { 1,  3, -0.99962954584931E-1},
            { 1,  7, -0.32632037778459E-2},
            { 1,  8,  0.23320922576723E-3},
            { 2,  0, -0.15334809857450E+0},
            { 2,  1,  0.29072288239902E-1},
            { 2,  5,  0.37534702741167E-3},
            { 3,  0,  0.17296691702411E-2},
            { 3,  1, -0.38556050844504E-3},
            { 3,  3, -0.35017712292608E-4},
            { 4,  0, -0.14566393631492E-4},
            { 4,  1,  0.56420857267269E-5},
            { 5,  0,  0.41286150074605E-7},
            { 5,  1, -0.20684671118824E-7},
            { 5,  2,  0.16409393674725E-8}
        };

        static BackwardRegionResidualElement Coeff2cS[] = {
            {-2, 0,  0.90968501005365E+03},
            {-2, 1,  0.24045667088420E+04},
            {-1, 0, -0.59162326387130E+03},
            { 0, 0,  0.54145404128074E+03},
            { 0, 1, -0.27098308411192E+03},
            { 0, 2,  0.97976525097926E+03},
            { 0, 3, -0.46966772959435E+03},
            { 1, 0,  0.14399274604723E+02},
            { 1, 1, -0.19104204230429E+02},
            { 1, 3,  0.53299167111971E+01},
            { 1, 4, -0.21252975375934E+02},
            { 2, 0, -0.31147334413760E+00},
            { 2, 1,  0.60334840894623E+00},
            { 2, 2, -0.42764839702509E-01},
            { 3, 0,  0.58185597255259E-02},
            { 3, 1, -0.14597008284753E-01},
            { 3, 5,  0.56631175631027E-02},
            { 4, 0, -0.76155864584577E-04},
            { 4, 1,  0.22440342919332E-03},
            { 4, 4, -0.12561095013413E-04},
            { 5, 0,  0.63323132660934E-06},
            { 5, 1, -0.20541989675375E-05},
            { 5, 2,  0.36405370390082E-07},
            { 6, 0, -0.29759897789215E-08},
            { 6, 1,  0.10136618529763E-07},
            { 7, 0,  0.59925719692351E-11},
            { 7, 1, -0.20677870105164E-10},
            { 7, 3, -0.20874278181886E-10},
            { 7, 4,  0.10162166825089E-09},
            { 7, 5, -0.16429828281347E-09}
        };

        static BackwardRegionResidualElement Coeff2aHS[] = {
            {0,  1, -0.182575361923032E-1},
            {0,  3, -0.125229548799536E+0},
            {0,  6,  0.592290437320145E+0},
            {0, 16,  0.604769706185122E+1},
            {0, 20,  0.238624965444474E+3},
            {0, 22, -0.298639090222922E+3},
            {1,  0,  0.512250813040750E-1},
            {1,  1, -0.437266515606486E+0},
            {1,  2,  0.413336902999504E+0},
            {1,  3, -0.516468254574773E+1},
            {1,  5, -0.557014838445711E+1},
            {1,  6,  0.128555037824478E+2},
            {1, 10,  0.114144108953290E+2},
            {1, 16, -0.119504225652714E+3},
            {1, 20, -0.284777985961560E+4},
            {1, 22,  0.431757846408006E+4},
            {2,  3,  0.112894040802650E+1},
            {2, 16,  0.197409186206319E+4},
            {2, 20,  0.151612444706087E+4},
            {3,  0,  0.141324451421235E-1},
            {3,  2,  0.585501282219601E+0},
            {3,  3, -0.297258075863012E+1},
            {3,  6,  0.594567314847319E+1},
            {3, 16, -0.623656565798905E+4},
            {4, 16,  0.965986235133332E+4},
            {5,  3,  0.681500934948134E+1},
            {5, 16, -0.633207286824489E+4},
            {6,  3, -0.558919224465760E+1},
            {7,  1,  0.400645798472063E-1}
        };

        static BackwardRegionResidualElement Coeff2bHS[] = {
            { 0,  0,  0.801496989929495E-01},
            { 0,  1, -0.543862807146111E+00},
            { 0,  2,  0.337455597421283E+00},
            { 0,  4,  0.890555451157450E+01},
            { 0,  8,  0.313840736431485E+03},
            { 1,  0,  0.797367065977789E+00},
            { 1,  1, -0.121616973556240E+01},
            { 1,  2,  0.872803386937477E+01},
            { 1,  3, -0.169769781757602E+02},
            { 1,  5, -0.186552827328416E+03},
            { 1, 12,  0.951159274344237E+05},
            { 2,  1, -0.189168510120494E+02},
            { 2,  6, -0.433407037194840E+04},
            { 2, 18,  0.543212633012715E+09},
            { 3,  0,  0.144793408386013E+00},
            { 3,  1,  0.128024559637516E+03},
            { 3,  7, -0.672309534071268E+05},
            { 3, 12,  0.336972380095287E+08},
            { 4,  1, -0.586634196762720E+03},
            { 4, 16, -0.221403224769889E+11},
            { 5,  1,  0.171606668708389E+04},
            { 5, 12, -0.570817595806302E+09},
            { 6,  1, -0.312109693178482E+04},
            { 6,  8, -0.207841384633010E+07},
            { 6, 18,  0.305605946157786E+13},
            { 7,  1,  0.322157004314333E+04},
            { 7, 16,  0.326810259797295E+12},
            { 8,  1, -0.144104158934487E+04},
            { 8,  3,  0.410694867802691E+03},
            { 8, 14,  0.109077066873024E+12},
            { 8, 18, -0.247964654258893E+14},
            {12, 10,  0.188801906865134E+10},
            {14, 16, -0.123651009018773E+15}
        };

        static BackwardRegionResidualElement Coeff2cHS[] = {
            { 0,  0,  0.112225607199012E+00},
            { 0,  1, -0.339005953606712E+01},
            { 0,  2, -0.320503911730094E+02},
            { 0,  3, -0.197597305104900E+03},
            { 0,  4, -0.407693861553446E+03},
            { 0,  8,  0.132943775222331E+05},
            { 1,  0,  0.170846839774007E+01},
            { 1,  2,  0.373694198142245E+02},
            { 1,  5,  0.358144365815434E+04},
            { 1,  8,  0.423014446424664E+06},
            { 1, 14, -0.751071025760063E+09},
            { 2,  2,  0.523446127607898E+02},
            { 2,  3, -0.228351290812417E+03},
            { 2,  7, -0.960652417056937E+06},
            { 2, 10, -0.807059292526074E+08},
            { 2, 18,  0.162698017225669E+13},
            { 3,  0,  0.772465073604171E+00},
            { 3,  5,  0.463929973837746E+05},
            { 3,  8, -0.137317885134128E+08},
            { 3, 16,  0.170470392630512E+13},
            { 3, 18, -0.251104628187308E+14},
            { 4, 18,  0.317748830835520E+14},
            { 5,  1,  0.538685623675312E+02},
            { 5,  4, -0.553089094625169E+05},
            { 5,  6, -0.102861522421405E+07},
            { 5, 14,  0.204249418756234E+13},
            { 6,  8,  0.273918446626977E+09},
            { 6, 18, -0.263963146312685E+16},
            {10,  7, -0.107890854108088E+10},
            {12,  7, -0.296492620980124E+11},
            {16, 10, -0.111754907323424E+16}
        };

        static BackwardRegionResidualElement Coeff3aH[] = {
            {-12,  0, -0.133645667811215E-6},
            {-12,  1,  0.455912656802978E-5},
            {-12,  2, -0.146294640700979E-4},
            {-12,  6,  0.639341312970080E-2},
            {-12, 14,  0.372783927268847E+3},
            {-12, 16, -0.718654377460447E+4},
            {-12, 20,  0.573494752103400E+6},
            {-12, 22, -0.267569329111439E+7},
            {-10,  1, -0.334066283302614E-4},
            {-10,  5, -0.245479214069597E-1},
            {-10, 12,  0.478087847764996E+2},
            { -8,  0,  0.764664131818904E-5},
            { -8,  2,  0.128350627676972E-2},
            { -8,  4,  0.171219081377331E-1},
            { -8, 10, -0.851007304583213E+1},
            { -5,  2, -0.136513461629781E-1},
            { -3,  0, -0.384460997596657E-5},
            { -2,  1,  0.337423807911655E-2},
            { -2,  3, -0.551624873066791E+0},
            { -2,  4,  0.729202277107470E+0},
            { -1,  0, -0.992522757376041E-2},
            { -1,  2, -0.119308831407288E+0},
            {  0,  0,  0.793929190615421E+0},
            {  0,  1,  0.454270731799386E+0},
            {  1,  1,  0.209998591259910E+0},
            {  3,  0, -0.642109823904738E-2},
            {  3,  1, -0.235155868604540E-1},
            {  4,  0,  0.252233108341612E-2},
            {  4,  3, -0.764885133368119E-2},
            { 10,  4,  0.136176427574291E-1},
            { 12,  5, -0.133027883575669E-1}
        };

        static BackwardRegionResidualElement Coeff3bH[] = {
            {-12,  0,  0.323254573644920E-4},
            {-12,  1, -0.127575556587181E-3},
            {-10,  0, -0.475851877356068E-3},
            {-10,  1,  0.156183014181602E-2},
            {-10,  5,  0.105724860113781E+0},
            {-10, 10, -0.858514221132534E+2},
            {-10, 12,  0.724140095480911E+3},
            { -8,  0,  0.296475810273257E-2},
            { -8,  1, -0.592721983365988E-2},
            { -8,  2, -0.126305422818666E-1},
            { -8,  4, -0.115716196364853E+0},
            { -8, 10,  0.849000969739595E+2},
            { -6,  0, -0.108602260086615E-1},
            { -6,  1,  0.154304475328851E-1},
            { -6,  2,  0.750455441524466E-1},
            { -4,  0,  0.252520973612982E-1},
            { -4,  1, -0.602507901232996E-1},
            { -3,  5, -0.307622221350501E+1},
            { -2,  0, -0.574011959864879E-1},
            { -2,  4,  0.503471360939849E+1},
            { -1,  2, -0.925081888584834E+0},
            { -1,  4,  0.391733882917546E+1},
            { -1,  6, -0.773146007130190E+2},
            { -1, 10,  0.949308762098587E+4},
            { -1, 14, -0.141043719679409E+7},
            { -1, 16,  0.849166230819026E+7},
            {  0,  0,  0.861095729446704E+0},
            {  0,  2,  0.323346442811720E+0},
            {  1,  1,  0.873281936020439E+0},
            {  3,  1, -0.436653048526683E+0},
            {  5,  1,  0.286596714529479E+0},
            {  6,  1, -0.131778331276228E+0},
            {  8,  1,  0.676682064330275E-2}
        };

        static BackwardRegionResidualElement Coeff3aS[] = {
            {-12.00, 28,  0.150042008263875E+10},
            {-12.00, 32, -0.159397258480424E+12},
            {-10.00,  4,  0.502181140217975E-03},
            {-10.00, 10, -0.672057767855466E+02},
            {-10.00, 12,  0.145058545404456E+04},
            {-10.00, 14, -0.823889534888890E+04},
            { -8.00,  5, -0.154852214233853E+00},
            { -8.00,  7,  0.112305046746695E+02},
            { -8.00,  8, -0.297000213482822E+02},
            { -8.00, 28,  0.438565132635495E+11},
            { -6.00,  2,  0.137837838635464E-02},
            { -6.00,  6, -0.297478527157462E+01},
            { -6.00, 32,  0.971777947349413E+13},
            { -5.00,  0, -0.571527767052398E-04},
            { -5.00, 14,  0.288307949778420E+05},
            { -5.00, 32, -0.744428289262703E+14},
            { -4.00,  6,  0.128017324848921E+02},
            { -4.00, 10, -0.368275545889071E+03},
            { -4.00, 36,  0.664768904779177E+16},
            { -2.00,  1,  0.449359251958880E-01},
            { -2.00,  4, -0.422897836099655E+01},
            { -1.00,  1, -0.240614376434179E+00},
            { -1.00,  6, -0.474341365254924E+01},
            {  0.00,  0,  0.724093999126110E+00},
            {  0.00,  1,  0.923874349695897E+00},
            {  0.00,  4,  0.399043655281015E+01},
            {  1.00,  0,  0.384066651868009E-01},
            {  2.00,  0, -0.359344365571848E-02},
            {  2.00,  3, -0.735196448821653E+00},
            {  3.00,  2,  0.188367048396131E+00},
            {  8.00,  0,  0.141064266818704E-03},
            {  8.00,  1, -0.257418501496337E-02},
            { 10.00,  2,  0.123220024851555E-02}
        };

        static BackwardRegionResidualElement Coeff3bS[] = {
            {-12,  1,  0.527111701601660E+00},
            {-12,  3, -0.401317830052742E+02},
            {-12,  4,  0.153020073134484E+03},
            {-12,  7, -0.224799398218827E+04},
            { -8,  0, -0.193993484669048E+00},
            { -8,  1, -0.140467557893768E+01},
            { -8,  3,  0.426799878114024E+02},
            { -6,  0,  0.752810643416743E+00},
            { -6,  2,  0.226657238616417E+02},
            { -6,  4, -0.622873556909932E+03},
            { -5,  0, -0.660823667935396E+00},
            { -5,  1,  0.841267087271658E+00},
            { -5,  2, -0.253717501764397E+02},
            { -5,  4,  0.485708963532948E+03},
            { -5,  6,  0.880531517490555E+03},
            { -4, 12,  0.265015592794626E+07},
            { -3,  1, -0.359287150025783E+00},
            { -3,  6, -0.656991567673753E+03},
            { -2,  2,  0.241768149185367E+01},
            {  0,  0,  0.856873461222588E+00},
            {  2,  1,  0.655143675313458E+00},
            {  3,  1, -0.213535213206406E+00},
            {  4,  0,  0.562974957606348E-02},
            {  5, 24, -0.316955725450471E+15},
            {  6,  0, -0.699997000152457E-03},
            {  8,  3,  0.119845803210767E-01},
            { 12,  1,  0.193848122022095E-04},
            { 14,  2, -0.215095749182309E-04}
        };

        static BackwardRegionResidualElement Coeff3aHS[] = {
            { 0,  0,  0.770889828326934E01},
            { 0,  1, -0.260835009128688E02},
            { 0,  5,  0.267416218930389E03},
            { 1,  0,  0.172221089496844E02},
            { 1,  3, -0.293542332145970E03},
            { 1,  4,  0.614135601882478E03},
            { 1,  8, -0.610562757725674E05},
            { 1, 14, -0.651272251118219E08},
            { 2,  6,  0.735919313521937E05},
            { 2, 16, -0.116646505914191E11},
            { 3,  0,  0.355267086434461E02},
            { 3,  2, -0.596144543825955E03},
            { 3,  3, -0.475842430145708E03},
            { 4,  0,  0.696781965359503E02},
            { 4,  1,  0.335674250377312E03},
            { 4,  4,  0.250526809130882E05},
            { 4,  5,  0.146997380630766E06},
            { 5, 28,  0.538069315091534E20},
            { 6, 28,  0.143619827291346E22},
            { 7, 24,  0.364985866165994E20},
            { 8,  1, -0.254741561156775E04},
            {10, 32,  0.240120197096563E28},
            {10, 36, -0.393847464679496E30},
            {14, 22,  0.147073407024852E25},
            {18, 28, -0.426391250432059E32},
            {20, 36,  0.194509340621077E39},
            {22, 16,  0.666212132114896E24},
            {22, 28,  0.706777016552858E34},
            {24, 36,  0.175563621975576E42},
            {28, 16,  0.108408607429124E29},
            {28, 36,  0.730872705175151E44},
            {32, 10,  0.159145847398870E25},
            {32, 28,  0.377121605943324E41}
        };

        static BackwardRegionResidualElement Coeff3bHS[] = {
            {-12,  2,  0.125244360717979E-12},
            {-12, 10, -0.126599322553713E-01},
            {-12, 12,  0.506878030140626E+01},
            {-12, 14,  0.317847171154202E+02},
            {-12, 20, -0.391041161399932E+06},
            {-10,  2, -0.975733406392044E-10},
            {-10, 10, -0.186312419488279E+02},
            {-10, 14,  0.510973543414101E+03},
            {-10, 18,  0.373847005822362E+06},
            { -8,  2,  0.299804024666572E-07},
            { -8,  8,  0.200544393820342E+02},
            { -6,  2, -0.498030487662829E-05},
            { -6,  6, -0.102301806360030E+02},
            { -6,  7,  0.552819126990325E+02},
            { -6,  8, -0.206211367510878E+03},
            { -5, 10, -0.794012232324823E+04},
            { -4,  4,  0.782248472028153E+01},
            { -4,  5, -0.586544326902468E+02},
            { -4,  8,  0.355073647696481E+04},
            { -3,  1, -0.115303107290162E-03},
            { -3,  3, -0.175092403171802E+01},
            { -3,  5,  0.257981687748160E+03},
            { -3,  6, -0.727048374179467E+03},
            { -2,  0,  0.121644822609198E-03},
            { -2,  1,  0.393137871762692E-01},
            { -1,  0,  0.704181005909296E-02},
            {  0,  3, -0.829108200698110E+02},
            {  2,  0, -0.265178818131250E+00},
            {  2,  1,  0.137531682453991E+02},
            {  5,  0, -0.522394090753046E+02},
            {  6,  1,  0.240556298941048E+04},
            {  8,  1, -0.227361631268929E+05},
            { 10,  1,  0.890746343932567E+05},
            { 14,  3, -0.239234565822486E+08},
            { 14,  7,  0.568795808129714E+10}
        };

        static BackwardRegionResidualElement Coeffb14HS[] = {
            { 0, 14,  0.332171191705237E+0},
            { 0, 36,  0.611217706323496E-3},
            { 1,  3, -0.882092478906822E+1},
            { 1, 16, -0.455628192543250E+0},
            { 2,  0, -0.263483840850452E-4},
            { 2,  5, -0.223949661148062E+2},
            { 3,  4, -0.428398660164013E+1},
            { 3, 36, -0.616679338856916E+0},
            { 4,  4, -0.146823031104040E+2},
            { 4, 16,  0.284523138727299E+3},
            { 4, 24, -0.113398503195444E+3},
            { 5, 18,  0.115671380760859E+4},
            { 5, 24,  0.395551267359325E+3},
            { 7,  1, -0.154891257229285E+1},
            { 8,  4,  0.194486637751291E+2},
            {12,  2, -0.357915139457043E+1},
            {12,  4, -0.335369414148819E+1},
            {14,  1, -0.664426796332460E+0},
            {14, 22,  0.323321885383934E+5},
            {16, 10,  0.331766744667084E+4},
            {20, 12, -0.223501257931087E+5},
            {20, 28,  0.573953875852936E+7},
            {22,  8,  0.173226193407919E+3},
            {24,  3, -0.363968822121321E-1},
            {28,  0,  0.834596332878346E-6},
            {32,  6,  0.503611916682674E+1},
            {32,  8,  0.655444787064505E+2}
        };

        static BackwardRegionResidualElement Coeffb3a4HS[] = {
            { 0,  1,  0.822673364673336E+0},
            { 0,  4,  0.181977213534479E+0},
            { 0, 10, -0.112000260313624E-1},
            { 0, 16, -0.746778287048033E-3},
            { 2,  1, -0.179046263257381E+0},
            { 3, 36,  0.424220110836657E-1},
            { 4,  3, -0.341355823438768E+0},
            { 4, 16, -0.209881740853565E+1},
            { 5, 20, -0.822477343323596E+1},
            { 5, 36, -0.499684082076008E+1},
            { 6,  4,  0.191413958471069E+0},
            { 7,  2,  0.581062241093136E-1},
            { 7, 28, -0.165505498701029E+4},
            { 7, 32,  0.158870443421201E+4},
            {10, 14, -0.850623535172818E+2},
            {10, 32, -0.317714386511207E+5},
            {10, 36, -0.945890406632871E+5},
            {32,  0, -0.139273847088690E-5},
            {32,  6,  0.631052532240980E+0}
        };

        static BackwardRegionResidualElement Coeffb2abHS[] = {
            { 1,  8, -0.524581170928788E03},
            { 1, 24, -0.926947218142218E07},
            { 2,  4, -0.237385107491666E03},
            { 2, 32,  0.210770155812776E11},
            { 4,  1, -0.239494562010986E02},
            { 4,  2,  0.221802480294197E03},
            { 7,  7, -0.510472533393438E07},
            { 8,  5,  0.124981396109147E07},
            { 8, 12,  0.200008436996201E10},
            {10,  1, -0.815158509791035E03},
            {12,  0, -0.157612685637523E03},
            {12,  7, -0.114200422332791E11},
            {18, 10,  0.662364680776872E16},
            {20, 12, -0.227622818296144E19},
            {24, 32, -0.171048081348406E32},
            {28,  8,  0.660788766938091E16},
            {28, 12,  0.166320055886021E23},
            {28, 20, -0.218003784381501E30},
            {28, 22, -0.787276140295618E30},
            {28, 24,  0.151062329700346E32},
            {32,  2,  0.795732170300541E07},
            {32,  7,  0.131957647355347E16},
            {32, 12, -0.325097068299140E24},
            {32, 14, -0.418600611419248E26},
            {32, 24,  0.297478906557467E35},
            {36, 10, -0.953588761745473E20},
            {36, 12,  0.166957699620939E25},
            {36, 20, -0.175407764869978E33},
            {36, 22,  0.347581490626396E35},
            {36, 28, -0.710971318427851E39}
        };

        static BackwardRegionResidualElement Coeffb2c3bHS[] = {
            { 0,  0,  0.104351280732769E01},
            { 0,  3, -0.227807912708513E01},
            { 0,  4,  0.180535256723202E01},
            { 1,  0,  0.420440834792042E00},
            { 1, 12, -0.105721244834660E06},
            { 5, 36,  0.436911607493884E25},
            { 6, 12, -0.328032702839753E12},
            { 7, 16, -0.678686760804270E16},
            { 8,  2,  0.743957464645363E04},
            { 8, 20, -0.356896445355761E20},
            {12, 32,  0.167590585186801E32},
            {16, 36, -0.355028625419105E38},
            {22,  2,  0.396611982166538E12},
            {22, 32, -0.414716268484468E41},
            {24,  7,  0.359080103867382E19},
            {36, 20, -0.116994334851995E41}
        };

        static BackwardRegionResidualElement Coeffb13HS[] = {
            { 0,   0,  0.913965547600543E+00},
            { 1,  -2, -0.430944856041991E-04},
            { 1,   2,  0.603235694765419E+02},
            { 3, -12,  0.117518273082168E-17},
            { 5,  -4,  0.220000904781292E+00},
            { 6,  -3, -0.690815545851641E+02}
        };

        static BackwardRegionResidualElement CoeffTb23HS[] = {
            {-12,  10,  0.629096260829810E-03},
            {-10,   8, -0.823453502583165E-03},
            { -8,   3,  0.515446951519474E-07},
            { -4,   4, -0.117565945784945E+01},
            { -3,   3,  0.348519684726192E+01},
            { -2,  -6, -0.507837382408313E-11},
            { -2,   2, -0.284637670005479E+01},
            { -2,   3, -0.236092263939673E+01},
            { -2,   4,  0.601492324973779E+01},
            {  0,   0,  0.148039650824546E+01},
            {  1,  -3,  0.360075182221907E-03},
            {  1,  -2, -0.126700045009952E-01},
            {  1,  10, -0.122184332521413E+07},
            {  3,  -2,  0.149276502463272E+00},
            {  3,  -1,  0.698733471798484E+00},
            {  5,  -5, -0.252207040114321E-01},
            {  6,  -6,  0.147151930985213E-01},
            {  6,  -3, -0.108618917681849E+01},
            {  8,  -8, -0.936875039816322E-03},
            {  8,  -2,  0.819877897570217E+02},
            {  8,  -1, -0.182041861521835E+03},
            { 12, -12,  0.261907376402688E-05},
            { 12,  -1, -0.291626417025961E+05},
            { 14, -12,  0.140660774926165E-04},
            { 14,  -1,  0.783237062349385E+07}
        };

        static BackwardRegionResidualElement CoeffT4HS[] = {
            { 0,  0,  0.179882673606601E+00},
            { 0,  3, -0.267507455199603E+00},
            { 0, 12,  0.116276722612600E+01},
            { 1,  0,  0.147545428713616E+00},
            { 1,  1, -0.512871635973248E+00},
            { 1,  2,  0.421333567697984E+00},
            { 1,  5,  0.563749522189870E+00},
            { 2,  0,  0.429274443819153E+00},
            { 2,  5, -0.335704552142140E+01},
            { 2,  8,  0.108890916499278E+02},
            { 3,  0, -0.248483390456012E+00},
            { 3,  2,  0.304153221906390E+00},
            { 3,  3, -0.494819763939905E+00},
            { 3,  4,  0.107551674933261E+01},
            { 4,  0,  0.733888415457688E-01},
            { 4,  1,  0.140170545411085E-01},
            { 5,  1, -0.106110975998808E+00},
            { 5,  2,  0.168324361811875E-01},
            { 5,  4,  0.125028363714877E+01},
            { 5, 16,  0.101316840309509E+04},
            { 6,  6, -0.151791558000712E+01},
            { 6,  8,  0.524277865990866E+02},
            { 6, 22,  0.230495545563912E+05},
            { 8,  1,  0.249459806365456E-01},
            {10, 20,  0.210796467412137E+07},
            {10, 36,  0.366836848613065E+09},
            {12, 24, -0.144814105365163E+09},
            {14,  1, -0.179276373003590E-02},
            {14, 28,  0.489955602100459E+10},
            {16, 12,  0.471262212070518E+03},
            {16, 32, -0.829294390198652E+11},
            {18, 14, -0.171545662263191E+04},
            {18, 22,  0.355777682973575E+07},
            {18, 36,  0.586062760258436E+12},
            {20, 24, -0.129887635078195E+08},
            {28, 36,  0.317247449371057E+11}
        };

        class BackwardsRegion{
        protected:
            double p_star, X_star, T_star, h_star, s_star, s2_star;
            std::size_t N;
            double a, b, c, d, e, f;
            std::vector<double> I, J;
            std::vector<double> n;
        public:
    
            BackwardsRegion(BackwardRegionResidualElement data[], std::size_t N){
                this->N = N;
                for (std::size_t i = 0; i < N; ++i){
                    n.push_back(data[i].n);
                    I.push_back(data[i].I);
                    J.push_back(data[i].J);
                }
            };

            // This function imitates the Region3BackwardsRegion structure already written above
            // for v(T,p) in Region 3. However, it can be used for the functions T(p,h) [Y=T, X=h] or
            // T(p,s) [Y=T, X=s] in Regions 1, 2, or 3.  Additionally, it can be called for
            // v(p,h) [Y=v, X=h] or v(p,s) [Y=v, X=s] in Region 3.  since there are no direct formulas
            // for v(p,h) and v(p,s) in Regions 1 and 2, they have got be evaluated in a two step
            // process by evaluating v(T,p) using T(p,X) and the p value supplied.
            virtual double T_pX(double p, double X) const{
                const double pi = p/p_star, eta = X/X_star;
                double summer = 0;
                for (std::size_t i = 0; i < N; ++i){
                    summer += n[i]*pow(pi+a, I[i])*pow(eta+b, J[i])*pow(f, J[i]);
                }
                return summer*T_star;
            };

            // This function implements the backward boundary formulas for h'(s), h"(s) as defined 
            // in the IAPWS supplementary releases of 2014 for region 3.  It should only be called
            // when the appropriate coefficients are provided for an h(s) instance of this class.
            virtual double h_s(double s) const{
                const double sigma1 = s/s_star, sigma2 = s/s2_star;
                double summer = 0;
                for (std::size_t i = 0; i < N; ++i){
                    summer += n[i]*pow(pow(sigma1,d)+a, I[i])*pow(sigma2+b, J[i]);
                }
                // NOTE: c=1, e=0 : Straight summation
                //       c>1, e=0 : Power fit
                //       c=1, e=1 : Exp fit
                return ( (1-e)*pow(summer,c) + e*exp(summer) )*h_star;
            };

            // This function implements the backward formulas for p(h,s) as defined in the IAPWS
            // supplementary releases of 2014 for regions 1, 2, and 3.  It should only be called
            // when the appropriate coefficients are provided for a p(h,s) instance of this class.
            virtual double p_hs(double h, double s) const{
                const double eta = h/h_star, sigma = s/s_star;
                double summer = 0;
                for (std::size_t i = 0; i < N; ++i){
                    summer += n[i]*pow(eta+a, I[i])*pow(sigma+b, J[i]);
                }
                return pow(summer,c)*p_star;
            };

//       Add function for Tb23(h,s) Boundary Line.  There will only be one instance below.
//       It may double as Tsat(h,s) in Region 4 as well.
            virtual double t_hs(double h, double s) const{
                const double eta = h/h_star, sigma = s/s_star;
                double summer = 0;
                for (std::size_t i = 0; i < N; ++i){
                    summer += n[i]*pow(eta+a, I[i])*pow(sigma+b, J[i]);
                }
                return summer*T_star;
            };

        };  // BackwardsRegion Class

        // Region 1 *******************************************************************************
        class Region1H : public BackwardsRegion{
        public:
            Region1H() : BackwardsRegion(Coeff1H, 20){ 
                p_star = 1*p_fact; T_star = 1.0; X_star = 2500.0*R_fact; a = 0; b = 1.0; f = 1;
            };
        };
        class Region1S : public BackwardsRegion{
        public:
            Region1S() : BackwardsRegion(Coeff1S, 20){ 
                p_star = 1*p_fact; T_star = 1.0; X_star = 1.0*R_fact; a = 0; b = 2.0; f = 1;
            };
        };
        class Region1HS : public BackwardsRegion{
        public:
            Region1HS() : BackwardsRegion(Coeff1HS, 19){ 
                p_star = 100*p_fact; h_star = 3400*R_fact; s_star = 7.6*R_fact; a = 0.05; b = 0.05; c = 1;
            };
        };
        // Region 2 *******************************************************************************
        class Region2aH : public BackwardsRegion{
        public:
            Region2aH() : BackwardsRegion(Coeff2aH, 34){ 
                p_star = 1*p_fact; T_star = 1.0; X_star = 2000.0*R_fact; a = 0; b = -2.1; f = 1;
            };
        };
        class Region2bH : public BackwardsRegion{
        public:
            Region2bH() : BackwardsRegion(Coeff2bH, 38){ 
                p_star = 1*p_fact; T_star = 1.0; X_star = 2000.0*R_fact; a = -2; b = -2.6; f = 1;
            };
        };
        class Region2cH : public BackwardsRegion{
        public:
            Region2cH() : BackwardsRegion(Coeff2cH, 23){ 
                p_star = 1*p_fact; T_star = 1.0; X_star = 2000.0*R_fact; a = 25; b = -1.8; f = 1;
            };
        };
        class Region2aS : public BackwardsRegion{
        public:
            Region2aS() : BackwardsRegion(Coeff2aS, 46){ 
                p_star = 1*p_fact; T_star = 1.0; X_star = 2.0*R_fact; a = 0; b = -2; f = 1;
            };
        };
        class Region2bS : public BackwardsRegion{
        public:
            Region2bS() : BackwardsRegion(Coeff2bS, 44){ 
                p_star = 1*p_fact; T_star = 1.0; X_star = 0.7853*R_fact; a = 0; b = -10; f = -1;
            };
        };
        class Region2cS : public BackwardsRegion{
        public:
            Region2cS() : BackwardsRegion(Coeff2cS, 30){ 
                p_star = 1*p_fact; T_star = 1.0; X_star = 2.9251*R_fact; a = 0; b = -2; f = -1;
            };
        };
        class Region2aHS : public BackwardsRegion{
        public:
            Region2aHS() : BackwardsRegion(Coeff2aHS, 29){ 
                p_star = 4*p_fact; h_star = 4200*R_fact; s_star = 12*R_fact; a = -0.5; b = -1.2; c = 4;
            };
        };
        class Region2bHS : public BackwardsRegion{
        public:
            Region2bHS() : BackwardsRegion(Coeff2bHS, 33){ 
                p_star = 100*p_fact; h_star = 4100*R_fact; s_star = 7.9*R_fact; a = -0.6; b = -1.01; c = 4;
            };
        };
        class Region2cHS : public BackwardsRegion{
        public:
            Region2cHS() : BackwardsRegion(Coeff2cHS, 31){ 
                p_star = 100*p_fact; h_star = 3500*R_fact; s_star = 5.9*R_fact; a = -0.7; b = -1.1; c = 4;
            };
        };
        // Region 3 *******************************************************************************
        class Region3aH : public BackwardsRegion{
        public:
            Region3aH() : BackwardsRegion(Coeff3aH, 31){ 
                p_star = 100*p_fact; T_star = 760.0; X_star = 2300.0*R_fact; a = 0.240; b = -0.615; f = 1;
            };
        };
        class Region3bH : public BackwardsRegion{
        public:
            Region3bH() : BackwardsRegion(Coeff3bH, 33){ 
                p_star = 100*p_fact; T_star = 860.0; X_star = 2800.0*R_fact; a = 0.298; b = -0.720; f = 1;
            };
        };
        class Region3aS : public BackwardsRegion{
        public:
            Region3aS() : BackwardsRegion(Coeff3aS, 33){ 
                p_star = 100*p_fact; T_star = 760.0; X_star = 4.4*R_fact; a = 0.240; b = -0.703; f = 1;
            };
        };
        class Region3bS : public BackwardsRegion{
        public:
            Region3bS() : BackwardsRegion(Coeff3bS, 28){ 
                p_star = 100*p_fact; T_star = 860.0; X_star = 5.3*R_fact; a = 0.760; b = -0.818; f = 1;
            };
        };
        class Region3aHS : public BackwardsRegion{
        public:
            Region3aHS() : BackwardsRegion(Coeff3aHS, 33){ 
                p_star = 99*p_fact; h_star = 2300*R_fact; s_star = 4.4*R_fact; a = -1.01; b = -0.750; c = 1;
            };
        };
        class Region3bHS : public BackwardsRegion{
        public:
            Region3bHS() : BackwardsRegion(Coeff3bHS, 35){ 
                p_star = 16.6*p_fact; h_star = 2800*R_fact; s_star = 5.3*R_fact; a = -0.681; b = -0.792; c = -1;
            };
        };
        // Region 4 *******************************************************************************
        class Region4HS : public BackwardsRegion{
        public:
            Region4HS() : BackwardsRegion(CoeffT4HS, 36){ 
                h_star = 2800.0*R_fact; s_star = 9.2*R_fact; T_star = 550; a = -0.119; b = -1.07;
            };
        };
        // h(s) Boundary Equations *******************************************************************************
        class Boundary14HS : public BackwardsRegion{
        public:
            Boundary14HS() : BackwardsRegion(Coeffb14HS, 27){ 
                h_star = 1700.0*R_fact; s_star = 3.8*R_fact; s2_star = s_star; a = -1.09; b = 0.366E-4; c = 1; d = 1; e = 0;
            };
        };
        class Boundary3a4HS : public BackwardsRegion{
        public:
            Boundary3a4HS() : BackwardsRegion(Coeffb3a4HS, 19){ 
                h_star = 1700.0*R_fact; s_star = 3.8*R_fact; s2_star = s_star; a = -1.09; b = 0.366E-4; c = 1; d = 1; e = 0;
            };
        };
        class Boundary2ab4HS : public BackwardsRegion{
        public:
            Boundary2ab4HS() : BackwardsRegion(Coeffb2abHS, 30){ 
                h_star = 2800.0*R_fact; s_star = 5.21*R_fact; s2_star = 9.2*R_fact; a = -0.513; b = -0.524; c = 1; d = -1; e = 1;
            };
        };
        class Boundary2c3b4HS : public BackwardsRegion{
        public:
            Boundary2c3b4HS() : BackwardsRegion(Coeffb2c3bHS, 16){ 
                h_star = 2800.0*R_fact; s_star = 5.9*R_fact; s2_star = s_star; a = -1.02; b = -0.726; c = 4; d = 1; e = 0;
            };
        };
        class Boundary13HS : public BackwardsRegion{
        public:
            Boundary13HS() : BackwardsRegion(Coeffb13HS, 6){ 
                h_star = 1700.0*R_fact; s_star = 3.8*R_fact; s2_star = s_star; a = -0.884; b = -0.864; c = 1; d = 1; e = 0;
            };
        };
        class Boundary23HS : public BackwardsRegion{
        public:
            Boundary23HS() : BackwardsRegion(CoeffTb23HS, 25){ 
                h_star = 3000.0*R_fact; s_star = 5.3*R_fact; T_star = 900; a = -0.727; b = -0.864;
            };
        };

        static double Region2b2cdata[] = {
	     0.90584278514723E+3,
	    -0.67955786399241E+0,
	     0.12809002730136E-3,
	     0.26526571908428E+4,
	     0.45257578905948E+1
        };

        static const std::vector<double> region2b2c_n(Region2b2cdata, Region2b2cdata + sizeof(Region2b2cdata)/sizeof(double));

        inline double P2b2c_h(double h){
            // Only called for Region determination and debugging.  No range checking.
            const double p_star = 1*p_fact, h_star = 1*R_fact, eta = h/h_star;
            const double PI = region2b2c_n[0] + region2b2c_n[1]*eta + region2b2c_n[2]*eta*eta;
            return PI*p_star;
        }
        inline double H2b2c_p(double p){
            // Only called for Region determination and debugging.  No range checking.
            const double p_star = 1*p_fact, h_star = 1*R_fact, PI = p/p_star;
            const double ETA = region2b2c_n[3] + sqrt((PI - region2b2c_n[4])/region2b2c_n[2]);
            return ETA*h_star;
        }

        static double Region3abdata[] = {
	     0.201464004206875E+4,
	     0.374696550136983E+1,
	    -0.219921901054187E-1,
	     0.875131686009950E-4,
        };

        static const std::vector<double> region3ab_n(Region3abdata, Region3abdata + sizeof(Region3abdata)/sizeof(double));

        inline double H3ab_p(double p){
            // Only called for Region determination and debugging.  No range checking.
            const double p_star = 1*p_fact, h_star = 1*R_fact, PI = p/p_star;
            double ETA = region3ab_n[0] + region3ab_n[1]*PI + region3ab_n[2]*PI*PI + region3ab_n[3]*PI*PI*PI;
            return ETA*h_star;
        };

        static double Region2abdata[] = {
	    -0.349898083432139E+4,
	     0.257560716905876E+4,
	    -0.421073558227969E+3,
	     0.276349063799944E+2,
        };

        static const std::vector<double> region2ab_n(Region2abdata, Region2abdata + sizeof(Region2abdata)/sizeof(double));

        inline double H2ab_s(double s){
            // Only called for Region determination and debugging.  No range checking.
            const double s_star = 1*R_fact, h_star = 1*R_fact, sigma = s/s_star;
            double ETA = region2ab_n[0] + region2ab_n[1]*sigma + region2ab_n[2]*pow(sigma,2) + region2ab_n[3]*pow(sigma,3);
            return ETA*h_star;
        };

        inline double H13_s(double s){
            // Only called for Region determination and debugging.  No range checking.
            static Boundary13HS b13; 
            return b13.h_s(s);
        };

        inline double Hsat_s(double s){
            // Only called for Region determination and debugging.  Has range checking.
            static Boundary14HS b14hs;
            static Boundary3a4HS b3a4hs;
            static Boundary2c3b4HS b2c3b4hs;
            static Boundary2ab4HS b2ab4hs;
            if (s < 0)
                throw std::out_of_range("Entropy out of range");
            else if (s <= SfT23 )
                return b14hs.h_s(s);
            else if (s <= Scrit)
                return b3a4hs.h_s(s);
            else if (s <= S2bc)
                return b2c3b4hs.h_s(s);
            else if (s <= Sgtrip)
                return b2ab4hs.h_s(s);
            else
                throw std::out_of_range("Entropy out of range");
        };

    };  // Backwards Namespace

    /********************************************************************************/
    /**************************      General          *******************************/
    /********************************************************************************/

    enum IF97REGIONS {REGION_1, REGION_2, REGION_3, REGION_4, REGION_5};
    enum IF97BACKREGIONS {BACK_1, BACK_2A, BACK_2B, BACK_2C, BACK_3A, BACK_3B, BACK_4};

    inline IF97REGIONS RegionDetermination_TP(double T, double p)
    {
        static Region4 R4;
        if (T > Text){
            throw std::out_of_range("Temperature out of range");
        }
        else if (T > Tmax && T <= Text){
            if (p <= Pext){
                return REGION_5;
            }
            else{
                throw std::out_of_range("Pressure out of range");
            }
        }
        else if (T > T23min && T <= Tmax){
            if (p > Pmax){
                throw std::out_of_range("Pressure out of range");
            }
            else if (p < 16.5292*p_fact){ // Check this one first to avoid the call to 2-3 boundary curve (a little bit faster)
                return REGION_2;
            }
            else if (p > Region23_T(T)){
                return REGION_3;
            }
            else{
                return REGION_2;
            }
        }
        else if (T >= Tmin && T <= T23min){
            if (p > Pmax)
                throw std::out_of_range("Pressure out of range");
            else if(p > R4.p_T(T))
                return REGION_1;
            else if(p < R4.p_T(T))
                return REGION_2;
            else
                return REGION_4;
        }
        else{
            throw std::out_of_range("Temperature out of range");
        }
    };

    inline double RegionOutput(IF97parameters outkey, double T, double p, IF97SatState State){
        static Region1 R1;
        static Region2 R2;
        static Region3 R3;
        static Region4 R4;
        static Region5 R5;

        IF97REGIONS region = RegionDetermination_TP(T, p);

        switch (region){
            case REGION_1: if (State == VAPOR) 
                               return R2.output(outkey, T, p);  // On saturation curve and need the Vapor phase
                           else
                               return R1.output(outkey, T, p);  // otherwise, use Liquid Region 1
                           break;
            case REGION_2: if (State == LIQUID)
                               return R1.output(outkey, T, p);  // On saturation curve and need the Liquid phase
                           else
                               return R2.output(outkey, T, p);  // otherwise, use Vapor Region 2
                           break;
            case REGION_3: return R3.output(outkey, T, p, State);
                           break;
            case REGION_4: if (State == VAPOR) {
                               return R2.output(outkey, T, p);
                           } else if (State == LIQUID) {
                               return R1.output(outkey, T, p);
                           } else {
                               throw std::out_of_range("Cannot use Region 4 with T and p as inputs");
                           }
                           break;
            case REGION_5: return R5.output(outkey, T, p);
        }
        throw std::out_of_range("Unable to match region");
    };


    inline IF97REGIONS RegionDetermination_pX(double p, double X, IF97parameters inkey){
        // Setup needed Region Equations for region determination
        static Region1 R1;
        static Region2 R2;
        // Saturation Region Limit Variables
        double Tsat = 0;
        double Xliq = 0;
        double Xvap = 0;

        // Check overall boundary limits
        if ((p < Pmin) || (p > Pmax))
                throw std::out_of_range("Pressure out of range");
        double Xmin = R1.output(inkey,Tmin,p);
        double Xmax = R2.output(inkey,Tmax,p);
        if (( X < Xmin ) || (X > (Xmax + 1.0E-10) )){
            if (inkey == IF97_HMASS){
                throw std::out_of_range("Enthalpy out of range");
            }
            else{
                throw std::out_of_range("Entropy out of range");
            }
        }

        // Check saturation Dome first
        if (p <= Pcrit) {
            Tsat = Tsat97(p);
            Xliq = R1.output(inkey,Tsat,p);
            Xvap = R2.output(inkey,Tsat,p);
            if ((Xliq <= X) && (X <= Xvap)){    // Within Saturation Dome
                return REGION_4;               //    Region 4
            }
        }
        // End Check saturation Dome

        // Check values below 16.529 MPa
        if (p <= P23min) {                        // p <= P23min (saturation dome)
            if (X <= Xliq) return REGION_1;
            else if (X >= Xvap) return REGION_2;
            else return REGION_4;
        } 
        // Check values above 16.529 MPa
        else if (X <= R1.output(inkey,T23min,p))
                return REGION_1;
        else if (X >= R2.output(inkey,Region23_p(p),p))
                return REGION_2;
        else
                return REGION_3;
    };  // Region Output backward


    inline int BackwardRegion(double p, double X, IF97parameters inkey){
        // This routine is for testing purposes only.  It returns the
        // Region as an integer based on the backward evaluation of either
        // (p,h) or (p,s)

        // Make sure input and output keys are valid for Backward formulas
        if ((inkey != IF97_HMASS) && (inkey != IF97_SMASS))
            throw std::invalid_argument("Backward Formulas take variable inputs of Enthalpy or Entropy only.");

        IF97REGIONS region = RegionDetermination_pX(p, X, inkey);

        switch(region){
        case REGION_1: return 1; break;
        case REGION_2: return 2; break;
        case REGION_3: return 3; break;
        case REGION_4: return 4; break;
        default: return 0; break;
        }
    }

    inline double RegionOutputBackward(double p, double X, IF97parameters inkey){
        // Note that this routine returns only temperature (IF97_T).  All other values should be
        // calculated from this temperature and the known pressure using forward equations.
        // Setup Backward Regions for output
        static Backwards::Region1H B1H;
        static Backwards::Region1S B1S;
        static Backwards::Region2aH B2aH;
        static Backwards::Region2bH B2bH;
        static Backwards::Region2cH B2cH;
        static Backwards::Region2aS B2aS;
        static Backwards::Region2bS B2bS;
        static Backwards::Region2cS B2cS;
        static Backwards::Region3aH B3aH;
        static Backwards::Region3bH B3bH;
        static Backwards::Region3aS B3aS;
        static Backwards::Region3bS B3bS;

        // Make sure input and output keys are valid for Backward formulas
        if ((inkey != IF97_HMASS) && (inkey != IF97_SMASS))
            throw std::invalid_argument("Backward Formulas take variable inputs of Enthalpy or Entropy only.");

        // Get Saturation Parameters

        IF97REGIONS region = RegionDetermination_pX(p, X, inkey);

        switch (region){
        case REGION_1: if (inkey == IF97_HMASS) 
                           return B1H.T_pX(p,X);
                       else
                           return B1S.T_pX(p,X);
                       break;
        case REGION_2: if (inkey == IF97_HMASS){
                           if (p <= 4.0*p_fact)
                               return B2aH.T_pX(p,X);
                           else if (X >= Backwards::H2b2c_p(p))
                               return B2bH.T_pX(p,X);
                           else
                               return B2cH.T_pX(p,X);
                       } else {
                           if (p <= 4.0*p_fact)
                               return B2aS.T_pX(p,X);
                           else if (X >= S2bc)
                               return B2bS.T_pX(p,X);
                           else 
                               return B2cS.T_pX(p,X);
                       }; break;
        case REGION_3: if (inkey == IF97_HMASS){
                           if (X <= Backwards::H3ab_p(p))
                               return B3aH.T_pX(p,X);
                           else
                               return B3bH.T_pX(p,X);
                       } else {
                           if (X <= Scrit)
                               return B3aS.T_pX(p,X);
                           else
                               return B3bS.T_pX(p,X);
                       }; break;
        case REGION_4: return Tsat97(p); break;
        default: throw std::out_of_range("Unable to match region");
        }
    }  // Region Output backward

    inline double rho_pX(double p, double X, IF97parameters inkey){
        // NOTE: This implementation works, and with the 2016 Supplementary Release
        //       for v(p,T) for Region 3 implemented, it is no longer iterative.  
        //       However, the 2014 Supplementary Release for v(p,h) and v(p,s) are 
        //       more direct and may be slightly faster, since only one algebraic 
        //       equation is needed instead of two in Region 3.
        static Region1 R1;
        static Region2 R2;
        const double T = RegionOutputBackward( p, X, inkey); 
        if (RegionDetermination_pX(p, X, inkey) == REGION_4){      // If in saturation dome
            const double Tsat = Tsat97(p);
            const double Xliq = R1.output(inkey,Tsat,p);
            const double Xvap = R2.output(inkey,Tsat,p);
            const double vliq = 1.0/R1.output(IF97_DMASS,Tsat,p);
            const double vvap = 1.0/R2.output(IF97_DMASS,Tsat,p);
            return 1.0/(vliq + (X-Xliq)*(vvap-vliq)/(Xvap-Xliq));  //    Return Mixture Density
        } else {                                                   // else
            return RegionOutput(IF97_DMASS, T, p, NONE);
        }
    }

    inline double Q_pX(double p, double X, IF97parameters inkey){
        double Xliq, Xvap;
        if ((p<Pmin) || (p>Pmax)) {
            throw std::out_of_range("Pressure out of range");
        } else if (p<Ptrip) {
            return 0;  //Liquid, at all temperatures
        } else if (p>Pcrit) { 
            double t;
            switch (inkey) {
            case IF97_HMASS:
            case IF97_SMASS:
                t = RegionOutputBackward( p, X, inkey); break;
            case IF97_UMASS:
            case IF97_DMASS:
            default:
                // There are no reverse functions for t(p,U) or t(p,rho)
                throw std::invalid_argument("Quality cannot be determined for these inputs.");
            }
            if (t<Tcrit)
                return 1.0;  // Vapor, at all pressures above critical point
            else
                // Supercritical Region (p>Pcrit) && (t>Tcrit)
                throw std::invalid_argument("Quality not defined in supercritical region.");
        } else {
            switch (inkey) {
                case IF97_HMASS:
                case IF97_SMASS:
                case IF97_UMASS:
                    Xliq = RegionOutput( inkey, Tsat97(p), p, LIQUID);
                    Xvap = RegionOutput( inkey, Tsat97(p), p, VAPOR);
                    return std::min(1.0,std::max(0.0,(X-Xliq)/(Xvap-Xliq)));
                    break;
                case IF97_DMASS:
                    Xliq = 1.0/RegionOutput( IF97_DMASS, Tsat97(p), p, LIQUID);
                    Xvap = 1.0/RegionOutput( IF97_DMASS, Tsat97(p), p, VAPOR);
                    X = 1.0/X;
                    return std::min(1.0,std::max(0.0,(X-Xliq)/(Xvap-Xliq)));
                    break;
                default:
                    throw std::invalid_argument("Quality cannot be determined for these inputs.");
            };
        }
        // If all else fails, which it shouldn't...
        throw std::invalid_argument("Quality cannot be determined for these inputs.");
        return -1;  // Should never occur, but eliminates warnings.
    };

    inline double X_pQ(IF97parameters inkey, double p, double Q){
        double Xliq, Xvap;
        if ((p<Ptrip) || (p>Pcrit)) 
            throw std::out_of_range("Pressure out of range");
        if ((Q<0.0) || (Q>1.0))
            throw std::out_of_range("Quality out of range");
        switch (inkey) {
            case IF97_HMASS:
            case IF97_SMASS:
            case IF97_UMASS:
                Xliq = RegionOutput( inkey, Tsat97(p), p, LIQUID);
                Xvap = RegionOutput( inkey, Tsat97(p), p, VAPOR);
                return Q*Xvap + (1-Q)*Xliq;
                break;
            case IF97_DMASS:
                Xliq = 1.0/RegionOutput( IF97_DMASS, Tsat97(p), p, LIQUID);
                Xvap = 1.0/RegionOutput( IF97_DMASS, Tsat97(p), p, VAPOR);
                return 1.0/(Q*Xvap + (1-Q)*Xliq);
                break;
            default: 
                throw std::invalid_argument("Mixture property undefined");
                return -1;  // Should never occur but eliminates warnings.
                break;
        };
        return -1;  // Should never occur but eliminates warnings.
    };


    static double HTmaxdata[] = {
        1.00645619394616E4,
        1.94706669580164E5,
        -4.67105212810782E5,
        -3.38175262587035E4
    };

    static const std::vector<double> Hmax_n(HTmaxdata, HTmaxdata + sizeof(HTmaxdata)/sizeof(double));

    inline double Hmax(double s){
    // This function covers the top and right domain boundaries of constant Pmax and Tmax
        const double s_star = 1*R_fact, h_star = 1*R_fact, sigma = s/s_star;
        if (s < STPmax)  // Use forward equation along Pmax using T(Pmax,s) as Temperature
            return RegionOutput( IF97_HMASS,RegionOutputBackward(Pmax,s,IF97_SMASS),Pmax, NONE);
        else { 
        // Determining H(s) along Tmax is difficult because there is no direct p(T,s) formulation.
        // This linear combination fit h(s)=a*ln(s)+b/s+c/s²+d is not perfect, but it's close
        // and can serve as a limit along that Tmax boundary. Coefficients in HTmaxdata above.
        // There is a better way to do this using Newton-Raphson on Tmax = T(p,s), but it is iterative and slow.
            double ETA = Hmax_n[0]*log(sigma) + Hmax_n[1]/sigma + Hmax_n[2]/pow(sigma,2) +Hmax_n[3];
            return ETA*h_star;
        }
    };

    inline double Hmin(double s){
        if (s < Sgtrip)  // Interpolate through Region 4
            return (s - Sftrip)*(Hgtrip-Hftrip)/(Sgtrip-Sftrip) + Hftrip;
        else             // Use forward equation along Pmin using T(Pmin,s) as Temperature
            return RegionOutput( IF97_HMASS, RegionOutputBackward(Pmin,s,IF97_SMASS), Pmin, NONE);
    };

    inline IF97BACKREGIONS RegionDetermination_HS(double h, double s){
        static Backwards::Boundary13HS b13;
        static Backwards::Boundary23HS b23hs;
        static Backwards::Region2cHS R2c;

        // Check Overall Boundaries
        if ( (s < Smin) || (s > Smax) ) 
            throw std::out_of_range("Entropy out of range");
        if ( (h > Hmax(s)) || (h < Hmin(s)) )
            throw std::out_of_range("Enthalpy out of range");

        // ============================================================================
        // Start at the low entropy curves and work our way up.
        // =================================== Region 1 Check =========================
        if (s <= SfT23){
            if (h < Backwards::Hsat_s(s))     //   If below Saturated Liquid Curve
                return BACK_4;                //       REGION 4
            else if (s < S13min)              //   If below H13 Curve
                return BACK_1;                //       REGION 1
            else {                            //   IF within H13 Curve (S13min < s < SfT23)
                if (h < b13.h_s(s))           //       below curve
                    return BACK_1;            //           REGION 1
                else                          //       above curve
                    return BACK_3A;           //           REGION 3 
                 }                            //
        } else if (s <= Scrit){  //========== Region 3a Check (S < Scrit) ==============
            if (h < Backwards::Hsat_s(s))     //  If below Saturated Liquid Curve
                return BACK_4;                //      REGION 4
            else                              //  If above curve
                return BACK_3A;               //      REGION 3(a)
        } else if (s <= S23min){  //========== Region 3b Check            ==============
            if (h < Backwards::Hsat_s(s))     //  If below Saturated Liquid Curve
                return BACK_4;                //      REGION 4
            else                              //  If above curve
                return BACK_3B;               //      REGION 3(a)
        } else if (s <= S23max){  //========== Region 3b/2c Check Along B23 Curve ======
            if (h < Backwards::Hsat_s(s))     //  if below Saturated Vapor Curve
                return BACK_4;                //      REGION 4
            else if (h < H23min)              //  if below bounding box
                return BACK_3B;               //      REGION 3(b)
            else if (h > H23max)              //  if above bounding box
                return BACK_2C;               //      REGION 2(c)
            else {                            //  Need to check TB23 Curve
                double TB23 = b23hs.t_hs(h,s);       //  Calc TB23(h,s)
                double PB23 = Region23_T(TB23);      //  Calc Corresponding PB23
                double P    = R2c.p_hs(h,s);         //  Calc P(h,s) using Region 2c
                if (P > PB23)                        //  Above B23 Curve
                    return BACK_3B;                  //      REGION 3(b)
                else                                 //  Below B23 Curve
                    return BACK_2C;                  //      REGION 2(c)
            } 
        } else if (s <= S2bc){   //========== Region 3b Check            ==============
            if (h < Backwards::Hsat_s(s))     //  If below Saturated Liquid Curve
                return BACK_4;                //      REGION 4
            else                              //  If above curve
                return BACK_2C;               //      REGION 2(c)
        } else if (s < Sgtrip) { //========== Region 2a/2b (s > S2bc) above Sat. Curve ==
            if (h < Backwards::Hsat_s(s))     //  If below Saturated Vapor Curve
                return BACK_4;                //      REGION_4
            else {                            //  If above Curve then
                if (h > Backwards::H2ab_s(s))   //      if h > h2ab(s) Curve (P=4 MPa)
                    return BACK_2B;             //          REGION 2(b)
                else                            //      if h < h2ab(s) Curve (P=4 MPa)
                    return BACK_2A;             //          REGION 2(a)
            }
        } else
            return BACK_2A;      //========== Region 2a fall thru ========================
    };  // Region Determination HS

    inline double BackwardOutputHS(IF97parameters outkey,double h, double s){
        // Note that this routine returns only temperature (IF97_T).  All other values should be
        // calculated from this temperature and the known pressure using forward equations.
        // Setup Backward Regions for output
        static Backwards::Region1HS B1HS;
        static Backwards::Region2aHS B2aHS;
        static Backwards::Region2bHS B2bHS;
        static Backwards::Region2cHS B2cHS;
        static Backwards::Region3aHS B3aHS;
        static Backwards::Region3bHS B3bHS;
        static Backwards::Region4HS B4HS;
        //
        double Pval, Tval;

        // Make sure output keys are valid for Backward_HS formulas
        if ((outkey != IF97_P) && (outkey != IF97_T))
            throw std::invalid_argument("Backward HS Formulas output Temperature or Pressure only.");

        // Get Saturation Parameters

        IF97BACKREGIONS region = RegionDetermination_HS(h, s);

        switch (region){
        case BACK_1:   Pval = B1HS.p_hs(h,s);  break;
        case BACK_2A:  Pval = B2aHS.p_hs(h,s); break;
        case BACK_2B:  Pval = B2bHS.p_hs(h,s); break;
        case BACK_2C:  Pval = B2cHS.p_hs(h,s); break;
        case BACK_3A:  Pval = B3aHS.p_hs(h,s); break;
        case BACK_3B:  Pval = B3bHS.p_hs(h,s); break;
        case BACK_4:   if (s >= SgT23)   // T(h,s) only defined over part of the 2-phase region
                           Tval = B4HS.t_hs(h,s);
                       else
                           throw std::out_of_range("Entropy out of range");
                       break;
        default: throw std::out_of_range("Unable to match region");
        }
        if (outkey == IF97_P)            // Returning Pressure (IF97_P)
            if (region == BACK_4)        //
                return psat97(Tval);     //       Not REGION 4, already have pressure
            else                         // 
                return Pval;             //       REGION 4, Calculate Psat from Tsat
                                         //
        else                             // ELSE Returning Temperature
            if (region == BACK_4)        //
                return Tval;                                     // REGION 4, already have Temperature
            else                                                 //
                return RegionOutputBackward(Pval,h,IF97_HMASS);  // Not REGION 4 Calc from Backward T(p,h)
    }  // Region Output backward

    // ******************************************************************************** //
    //                                     API                                          //
    // ******************************************************************************** //

    /// Get the mass density [kg/m^3] as a function of T [K] and p [Pa]
    inline double rhomass_Tp(double T, double p){ return RegionOutput( IF97_DMASS, T, p, NONE); };
    /// Get the mass enthalpy [J/kg] as a function of T [K] and p [Pa]
    inline double hmass_Tp(double T, double p){ return RegionOutput( IF97_HMASS, T, p, NONE); };
    /// Get the mass entropy [J/kg/K] as a function of T [K] and p [Pa]
    inline double smass_Tp(double T, double p){ return RegionOutput( IF97_SMASS, T, p, NONE); };
    /// Get the mass internal energy [J/kg] as a function of T [K] and p [Pa]
    inline double umass_Tp(double T, double p){ return RegionOutput( IF97_UMASS, T, p, NONE); };
    /// Get the mass constant-pressure specific heat [J/kg/K] as a function of T [K] and p [Pa]
    inline double cpmass_Tp(double T, double p){ return RegionOutput( IF97_CPMASS, T, p, NONE); };
    /// Get the mass constant-volume specific heat [J/kg/K] as a function of T [K] and p [Pa]
    inline double cvmass_Tp(double T, double p){ return RegionOutput( IF97_CVMASS, T, p, NONE); };
    /// Get the speed of sound [m/s] as a function of T [K] and p [Pa]
    inline double speed_sound_Tp(double T, double p){ return RegionOutput( IF97_W, T, p, NONE); };
    /// Get the [d(rho)/d(p)]T [kg/m³/Pa] as a function of T [K] and p [Pa]
    inline double drhodp_Tp(double T, double p){ return RegionOutput( IF97_DRHODP, T, p, NONE); };

    // ******************************************************************************** //
    //                            Transport Properties                                  //
    // ******************************************************************************** //

    /// Get the viscosity [Pa-s] as a function of T [K] and Rho [kg/m³]
    inline double visc_TRho(double T, double rho) {	
        // Since we have density, we don't need to determine the region for viscosity.
        static Region1 R1;  // All regions use base region equations for visc(T,rho).
        return R1.visc( T, rho );
    };
    /// Get the viscosity [Pa-s] as a function of T [K] and p [Pa]
    inline double visc_Tp(double T, double p) { return RegionOutput(IF97_MU, T, p, NONE); };
    /// Get the thermal conductivity [W/m-K] as a function of T [K] and p [Pa]
    inline double tcond_Tp(double T, double p) { return RegionOutput(IF97_K, T, p, NONE); };
    /// Calculate the Prandtl number [dimensionless] as a function of T [K] and p [Pa]
    inline double prandtl_Tp(double T, double p) { 
        return visc_Tp(T,p) * cpmass_Tp(T,p) * (1000/R_fact) / tcond_Tp(T,p);
    };

    // ******************************************************************************** //
    //                             Saturated Vapor/Liquid Functions                     //
    // ******************************************************************************** //
    /// Get the saturated liquid mass density [kg/m^3] as a function of p [Pa]
    inline double rholiq_p(double p){ return RegionOutput( IF97_DMASS, Tsat97(p), p, LIQUID); };
    /// Get the saturated vapor mass density [kg/m^3] as a function of p [Pa]
    inline double rhovap_p(double p){ return RegionOutput( IF97_DMASS, Tsat97(p), p, VAPOR); };
    // ******************************************************************************** //
    /// Get the saturated liquid mass enthalpy [J/kg] as a function of p [Pa]
    inline double hliq_p(double p){ return RegionOutput( IF97_HMASS, Tsat97(p), p, LIQUID); };
    /// Get the saturated vapor mass enthalpy [J/kg] as a function of p [Pa]
    inline double hvap_p(double p){ return RegionOutput( IF97_HMASS, Tsat97(p), p, VAPOR); };
    // ******************************************************************************** //
    /// Get the saturated liquid mass entropy [J/kg/K] as a function of p [Pa]
    inline double sliq_p(double p){ return RegionOutput( IF97_SMASS, Tsat97(p), p, LIQUID); };
    /// Get the saturated vapor mass entropy [J/kg/K] as a function of p [Pa]
    inline double svap_p(double p){ return RegionOutput( IF97_SMASS, Tsat97(p), p, VAPOR); };
    // ******************************************************************************** //
    /// Get the saturated liquid mass internal energy [J/kg] as a function of p [Pa]
    inline double uliq_p(double p){ return RegionOutput( IF97_UMASS, Tsat97(p), p, LIQUID); };
    /// Get the saturated vapor mass internal energy [J/kg] as a function of p [Pa]
    inline double uvap_p(double p){ return RegionOutput( IF97_UMASS, Tsat97(p), p, VAPOR); };
    // ******************************************************************************** //
    /// Get the saturated liquid mass isobaric specific heat [J/kg/K] as a function of T [K] and p [Pa]
    inline double cpliq_p(double p){ return RegionOutput( IF97_CPMASS, Tsat97(p), p, LIQUID); };
    /// Get the saturated vapor mass isobaric specific heat [J/kg/K] as a function of T [K] and p [Pa]
    inline double cpvap_p(double p){ return RegionOutput( IF97_CPMASS, Tsat97(p), p, VAPOR); };
    // ******************************************************************************** //
    /// Get the saturated liquid mass isochoric specific heat [J/kg/K] as a function of T [K] and p [Pa]
    inline double cvliq_p(double p){ return RegionOutput( IF97_CVMASS, Tsat97(p), p, LIQUID); };
    /// Get the saturated vapor mass isochoric specific heat [J/kg/K] as a function of T [K] and p [Pa]
    inline double cvvap_p(double p){ return RegionOutput( IF97_CVMASS, Tsat97(p), p, VAPOR); };
    // ******************************************************************************** //
    /// Get the saturated liquid speed of sound [m/s] as a function of T [K] and p [Pa]
    inline double speed_soundliq_p(double p){ return RegionOutput( IF97_W, Tsat97(p), p, LIQUID); };
    /// Get the saturated vapor speed of sound [m/s] as a function of T [K] and p [Pa]
    inline double speed_soundvap_p(double p){ return RegionOutput( IF97_W, Tsat97(p), p, VAPOR); };
    // ******************************************************************************** //
    /// Get the saturated liquid viscosity [Pa-s] as a function of p [Pa]
    inline double viscliq_p( double p) { return RegionOutput( IF97_MU, Tsat97(p), p, LIQUID); };
    /// Get the saturated vapor viscosity [Pa-s] as a function of p [Pa]
    inline double viscvap_p( double p) { return RegionOutput( IF97_MU, Tsat97(p), p, VAPOR); };
    // ******************************************************************************** //
    /// Get the saturated liquid thermal conductivity [W/m-K] as a function of p [Pa]
    inline double tcondliq_p( double p) { return RegionOutput( IF97_K, Tsat97(p), p, LIQUID); };
    /// Get the saturated vapor thermal conductivity [W/m-K] as a function of p [Pa]
    inline double tcondvap_p( double p) { return RegionOutput( IF97_K, Tsat97(p), p, VAPOR); };
    // ******************************************************************************** //
    /// Calculate the saturated liquid Prandtl number [dimensionless] as a function of p [Pa]
    inline double prandtlliq_p(double p) { return viscliq_p(p) * cpliq_p(p) * (1000/R_fact) / tcondliq_p(p); };
    /// Calculate the saturated vapor Prandtl number [dimensionless] as a function of p [Pa]
    inline double prandtlvap_p(double p) { return viscvap_p(p) * cpvap_p(p) * (1000/R_fact) / tcondvap_p(p); };


    // ******************************************************************************** //
    //                               2-Phase Functions                                  //
    // ******************************************************************************** //
    /// Get the saturation temperature [K] as a function of p [Pa]
    inline double Tsat97(double p){
        static Region4 R4;
        return R4.T_p(p);
    };
    /// Get the saturation pressure [Pa] as a function of T [K]
    inline double psat97(double T){
        static Region4 R4;
        return R4.p_T(T);
    };
    /// Get surface tension [N/m] as a function of T [K]
	inline double sigma97(double T){
		static Region4 R4;
		return R4.sigma_t(T);
	};
    // ******************************************************************************** //
    //                              Backward Functions                                  //
    // ******************************************************************************** //
    inline double T_phmass(double p,double h){
        return RegionOutputBackward( p, h, IF97_HMASS);
    };
    inline double rhomass_phmass(double p,double h){
        return rho_pX( p, h, IF97_HMASS);
    };
    inline double T_psmass(double p,double s){
        return RegionOutputBackward( p, s, IF97_SMASS);
    };
    inline double rhomass_psmass(double p,double s){
        return rho_pX( p, s, IF97_SMASS);
    };
    inline double p_hsmass(double h, double s){
        return BackwardOutputHS(IF97_P, h, s);
    };
    inline double T_hsmass(double h, double s){
        return BackwardOutputHS(IF97_T, h, s);
    };
    inline int Region_ph(double p, double h){
        return BackwardRegion( p, h, IF97_HMASS);
    };
    inline int Region_ps(double p, double s){
        return BackwardRegion( p, s, IF97_SMASS);
    };
    // ******************************************************************************** //
    //                              Trivial Functions                                   //
    // ******************************************************************************** //
    /// Get the Triple Point Temperature and Pressure
    inline double get_Ttrip(){ return Ttrip; };
    inline double get_ptrip(){ return Ptrip; };
    /// Get the Critical Point Temperature and Pressure and Density
    inline double get_Tcrit(){ return Tcrit; };
    inline double get_pcrit(){ return Pcrit; };
    inline double get_rhocrit(){ return Rhocrit; };
    /// Get the Max and Min Temperatures and Pressures
    inline double get_Tmin(){ return Tmin; };
    inline double get_Pmin(){ return Pmin; };
    inline double get_Tmax(){ return Tmax; };
    inline double get_Pmax(){ return Pmax; };
    /// Get physical constants
    inline double get_MW() { return MW; };
    inline double get_Rgas() { return Rgas; };
    inline double get_Acentric() { return -log10(psat97(0.7*Tcrit)/Pcrit) - 1; };
    // ******************************************************************************** //
    //                              Utility Functions                                   //
    // ******************************************************************************** //
    inline std::string get_if97_version() { 
#ifdef IAPWS_UNITS
        std::string VSTRING(IF97VERSION);
        VSTRING.append(" (IAPWS Units)");
        return VSTRING;
#else
        return IF97VERSION;
#endif
    };
    inline double hmass_pQ(double p,double Q){
        return X_pQ(IF97_HMASS, p, Q);
    };
    inline double umass_pQ(double p,double Q){
        return X_pQ(IF97_UMASS, p, Q);
    };
    inline double smass_pQ(double p,double Q){
        return X_pQ(IF97_SMASS, p, Q);
    };
    inline double v_pQ(double p,double Q){
        return 1.0/X_pQ(IF97_DMASS, p, Q);
    };
    inline double rhomass_pQ(double p,double Q){
        return X_pQ(IF97_DMASS, p, Q);
    };
    inline double Q_phmass(double p,double h){
        return Q_pX(p, h, IF97_HMASS);
    };
    inline double Q_pumass(double p,double u){
        return Q_pX(p, u, IF97_UMASS);
    };
    inline double Q_psmass(double p,double s){
        return Q_pX(p, s, IF97_SMASS);
    };
    inline double Q_prhomass(double p,double rho){
        return Q_pX(p, rho, IF97_DMASS);
    };
    inline double Q_pv(double p,double v){
        return Q_pX(p, 1.0/v, IF97_DMASS);
    };
/*************************************************************************/
/* Vapor Quality as a function of H and S cannot be implemented at this  */
/* time as there IAPWS has not released a backward formula that covers   */
/* the entire vapor dome.  T(H,S) is only currently defined for the      */
/* range of s > s"(T23sat).  Leaving this code here in case IAPWS        */
/* releases a fit for the entire vapor dome.                             */
/*************************************************************************/
/*  inline double Q_hsmass(double h, double s){
        double hliq, hvap;
        if ((s < Smin) || (s > Smax)) {
            throw std::out_of_range("Entropy out of range");
        } else if ((h < Hmin(s)) || (h > Hmax(s))) {
            throw std::out_of_range("Enthalpy out of range");
        }
        double p = BackwardOutputHS(IF97_P, h, s);
        double t = RegionOutputBackward( p, h, IF97_HMASS);
        if ((p > Pcrit) && (t > Tcrit)) {
            throw std::out_of_range("Temperature out of range");
        } else if (BackwardRegion(p, h, IF97_HMASS) == 4) {
            hliq = RegionOutput( IF97_HMASS, Tsat97(p), p, LIQUID);
            hvap = RegionOutput( IF97_HMASS, Tsat97(p), p, VAPOR);
            return std::min(1.0,std::max(0.0,(h-hliq)/(hvap-hliq)));
        } else if (p > Pcrit) {
            return 0.0;
        } else {
            return 1.0;
        }
    };                                                                   */
/*************************************************************************/

}; /* namespace IF97 */

#if defined(ENABLE_CATCH)

struct Region3BackwardsData{
    char region; double T, p, v;
};

static Region3BackwardsData _Table5[] = {
    // Table 5 data 
    {'A', 630, 50e6, 1.470853100e-3},
    {'A', 670, 80e6, 1.503831359e-3},
    {'B', 710, 50e6, 2.204728587e-3},
    {'B', 750, 80e6, 1.973692940e-3},
    {'C', 630, 20e6, 1.761696406e-3},
    {'C', 650, 30e6, 1.819560617e-3},
    {'D', 656, 26e6, 2.245587720e-3},
    {'D', 670, 30e6, 2.506897702e-3},
    {'E', 661, 26e6, 2.970225962e-3},
    {'E', 675, 30e6, 3.004627086e-3},
    {'F', 671, 26e6, 5.019029401e-3},
    {'F', 690, 30e6, 4.656470142e-3},
    {'G', 649, 23.6e6, 2.163198378e-3},
    {'G', 650, 24e6, 2.166044161e-3},
    {'H', 652, 23.6e6, 2.651081407e-3},
    {'H', 654, 24e6, 2.967802335e-3},
    {'I', 653, 23.6e6, 3.273916816e-3},
    {'I', 655, 24e6, 3.550329864e-3},
    {'J', 655, 23.5e6, 4.545001142e-3},
    {'J', 660, 24e6, 5.100267704e-3},
    {'K', 660, 23e6, 6.109525997e-3},
    {'K', 670, 24e6, 6.427325645e-3},
    {'L', 646, 22.6e6, 2.117860851e-3},
    {'L', 646, 23e6, 2.062374674e-3},
    {'M', 648.6, 22.6e6, 2.533063780e-3},
    {'M', 649.3, 22.8e6, 2.572971781e-3},
    {'N', 649.0, 22.6e6, 2.923432711e-3},
    {'N', 649.7, 22.8e6, 2.913311494e-3},
    {'O', 649.1, 22.6e6, 3.131208996e-3},
    {'O', 649.9, 22.8e6, 3.221160278e-3},
    {'P', 649.4, 22.6e6, 3.715596186e-3},
    {'P', 650.2, 22.8e6, 3.664754790e-3},
    {'Q', 640, 21.1e6, 1.970999272e-3},
    {'Q', 643, 21.8e6, 2.043919161e-3},
    {'R', 644, 21.1e6, 5.251009921e-3},
    {'R', 648, 21.8e6, 5.256844741e-3},
    {'S', 635, 19.1e6, 1.932829079e-3},
    {'S', 638, 20e6, 1.985387227e-3},
    {'T', 626, 17e6, 8.483262001e-3},
    {'T', 640, 20e6, 6.227528101e-3},
    // Table 11
    {'U', 644.6, 21.5e6, 2.268366647e-3},
    {'U', 646.1, 22e6, 2.296350553e-3},
    {'V', 648.6, 22.5e6, 2.832373260e-3},
    {'V', 647.9, 22.3e6, 2.811424405e-3},
    {'W', 647.5, 22.15e6, 3.694032281e-3},
    {'W', 648.1, 22.3e6, 3.622226305e-3},
    {'X', 648, 22.11e6, 4.528072649e-3},
    {'X', 649, 22.3e6, 4.556905799e-3},
    {'Y', 646.84, 22e6, 2.698354719e-3},
    {'Y', 647.05, 22.064e6, 2.717655648e-3},
    {'Z', 646.89, 22e6, 3.798732962e-3},
    {'Z', 647.15, 22.064e6, 3.701940010e-3},
};


inline void print_IF97_Table5()
{
    static std::vector<Region3BackwardsData> Table5(_Table5, _Table5 + sizeof(_Table5)/sizeof(Region3BackwardsData));
    for (std::size_t i = 0; i < Table5.size(); ++i){
        double v = IF97::Region3Backwards::Region3_v_TP(Table5[i].region, Table5[i].T, Table5[i].p);
        if (std::abs(v - Table5[i].v) > 1e-12){
            std::cout << Table5[i].region << " " << Table5[i].T << " " << Table5[i].p << " " << v << " " << Table5[i].v << std::endl;
        }
        char region = IF97::Region3Backwards::BackwardsRegion3RegionDetermination(Table5[i].T, Table5[i].p);
        if (region != Table5[i].region)
        {
            IF97::Region3Backwards::BackwardsRegion3RegionDetermination(Table5[i].T, Table5[i].p);
            std::cout << std::setprecision(10) << Table5[i].region << " " << region << " " << Table5[i].T << " " << Table5[i].p << " " << v << " " << Table5[i].v << std::endl;
        }
    }
};

struct Table3Data{
    IF97::Region3Backwards::DividingLineEnum region; 
    double p, T;
};

static Table3Data _Table3[] = {
    
    // Table 3
    {IF97::Region3Backwards::LINE_AB, 40e6, 6.930341408e2},
    {IF97::Region3Backwards::LINE_CD, 25e6, 6.493659208e2},
    {IF97::Region3Backwards::LINE_EF, 40e6, 7.139593992e2},
    {IF97::Region3Backwards::LINE_GH, 23e6, 6.498873759e2},
    {IF97::Region3Backwards::LINE_IJ, 23e6, 6.515778091e2},
    {IF97::Region3Backwards::LINE_JK, 23e6, 6.558338344e2},
    {IF97::Region3Backwards::LINE_MN, 22.8e6, 6.496054133e2},
    {IF97::Region3Backwards::LINE_OP, 22.8e6, 6.500106943e2},
    {IF97::Region3Backwards::LINE_QU, 22e6, 6.456355027e2},
    {IF97::Region3Backwards::LINE_RX, 22e6, 6.482622754e2},
    // Table 11
    {IF97::Region3Backwards::LINE_UV, 22.3e6, 6.477996121e2},
    {IF97::Region3Backwards::LINE_WX, 22.3e6, 6.482049480e2},
};
static std::vector<Table3Data> Table3(_Table3, _Table3 + sizeof(_Table3)/sizeof(Table3Data));

inline void print_boundary_line_Table3()
{
    for (std::size_t i = 0; i < Table3.size(); ++i){
        double T = IF97::Region3Backwards::DividingLine(Table3[i].region, Table3[i].p);
        if (std::abs(T - Table3[i].T) > 1e-7){
            printf("p: %g errT: %g\n", Table3[i].p, T - Table3[i].T);
        }
    }
};

#endif

#endif
