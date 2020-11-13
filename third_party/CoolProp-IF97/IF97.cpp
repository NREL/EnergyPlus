// This compiler flag causes input/output units on the Thermodynamic Properties in IF97.h
// to take pressure units of [MPa] and Energy units of [kJ] for comparison with IAPWS
// table values.  If not set, the default behavior of IF97.h is to use straight SI units
// with pressure in [Pa] and enthalpy in [J].
#define IAPWS_UNITS

#include "IF97.h"
#include <stdio.h>

int main(){

    using namespace IF97;

    printf("*****************************************************************\n");
    printf("******************** Verification Tables ************************\n");
    printf("* Tables below are printed for verification.  Unless otherwise  *\n");
    printf("* noted, tables are reproduced from the                         *\n");
    printf("* \"Revised Release on the IAPWS Industrial Formulation 1997\"    *\n");
    printf("* IAPWS R7-97(2012).                                            *\n");
    printf("*****************************************************************\n\n\n");


    double T1 = 300, T2 = 300, T3 = 500, p1 = 3, p2 = 80, p3 = 3;
    printf("*****************************************************************\n");
    printf("******************** Table 5 - Region 1 *************************\n");
    printf("*****************************************************************\n");
    printf("%5s %11.8e %11.8e %11.8e\n", "v", 1/rhomass_Tp(T1, p1), 1/rhomass_Tp(T2, p2), 1/rhomass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "h", hmass_Tp(T1, p1), hmass_Tp(T2, p2), hmass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "u", umass_Tp(T1, p1), umass_Tp(T2, p2), umass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "s", smass_Tp(T1, p1), smass_Tp(T2, p2), smass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "cp", cpmass_Tp(T1, p1), cpmass_Tp(T2, p2), cpmass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "w", speed_sound_Tp(T1, p1), speed_sound_Tp(T2, p2), speed_sound_Tp(T3, p3));
    printf("***************************************************************\n\n");
    
    T1 = 300, T2 = 700, T3 = 700, p1 = 0.0035, p2 = 0.0035, p3 = 30;
    printf("***************************************************************\n");
    printf("******************* Table 15 - Region 2 ***********************\n");
    printf("***************************************************************\n");
    printf("%5s %11.8e %11.8e %11.8e\n", "v", 1/rhomass_Tp(T1, p1), 1/rhomass_Tp(T2, p2), 1/rhomass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "h", hmass_Tp(T1, p1), hmass_Tp(T2, p2), hmass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "s", smass_Tp(T1, p1), smass_Tp(T2, p2), smass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "cp", cpmass_Tp(T1, p1), cpmass_Tp(T2, p2), cpmass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "w", speed_sound_Tp(T1, p1), speed_sound_Tp(T2, p2), speed_sound_Tp(T3, p3));
    printf("***************************************************************\n\n");

    T1 = 650, T2 = 650, T3 = 750, p1 = 25.5837018, p2 = 22.2930643, p3 = 78.3095639;
    printf("***************************************************************\n");
    printf("******************* Table 33* - Region 3 **********************\n");
    printf("***************************************************************\n");
    printf("%5s %11.8e %11.8e %11.8e\n", "rho", rhomass_Tp(T1, p1), rhomass_Tp(T2, p2), rhomass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "h", hmass_Tp(T1, p1), hmass_Tp(T2, p2), hmass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "s", smass_Tp(T1, p1), smass_Tp(T2, p2), smass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "cp", cpmass_Tp(T1, p1), cpmass_Tp(T2, p2), cpmass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "w", speed_sound_Tp(T1, p1), speed_sound_Tp(T2, p2), speed_sound_Tp(T3, p3));
    printf("***************************************************************\n");
    printf("* NOTE: This table is evaluated by first evaluating density    \n");
    printf("        from the three pressure values of Table 33 in the      \n");
    printf("        2007 Revised Release document, using the reverse       \n");
    printf("        formulas from the (2014) \"Revised Supplementary       \n");
    printf("        Release on Backward Equations for Specific Volume\".   \n");
    printf("        As a result, the values below will not be exactly      \n");
    printf("        the Table 33 values, but should be within +/-1.0E-6.  \n");
    printf("        of the published values.                               \n\n");

    T1 = 1500, T2 = 1500, T3 = 2000, p1 = 0.5, p2 = 30, p3 = 30;
    printf("***************************************************************\n");
    printf("******************* Table 42 - Region 5 ***********************\n");
    printf("***************************************************************\n");
    printf("%5s %11.8e %11.8e %11.8e\n", "v", 1/rhomass_Tp(T1, p1), 1/rhomass_Tp(T2, p2), 1/rhomass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "h", hmass_Tp(T1, p1), hmass_Tp(T2, p2), hmass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "s", smass_Tp(T1, p1), smass_Tp(T2, p2), smass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "cp", cpmass_Tp(T1, p1), cpmass_Tp(T2, p2), cpmass_Tp(T3, p3));
    printf("%5s %11.8e %11.8e %11.8e\n", "w", speed_sound_Tp(T1, p1), speed_sound_Tp(T2, p2), speed_sound_Tp(T3, p3));
    printf("***************************************************************\n\n\n");

    printf("**************** Reverse Functions T(p,h) & T(p,s) ********************\n\n");

    p1  = 3.0; p2 = 80; p3 = 80; double h1 = 500, h2 = 500, h3 = 1500;
    double s1 = 0.5, s2 = 0.5, s3 = 3;
    printf("_______________________________________________________________________\n");
    printf("                                Region 1                               \n");
    printf("_______________________________________________________________________\n");
    printf("              Table 7              |               Table 9             \n");
    printf("___________________________________|___________________________________\n");
    printf(" p/MPa  h/(kJ/kg)        T/K       | p/MPa s/(kJ/kg-K)       T/K       \n");
    printf("___________________________________|___________________________________\n");
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f     %3.1f      %11.8e \n",p1,h1,T_phmass(p1,h1),
                                                                            p1,s1,T_psmass(p1,s1));
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f     %3.1f      %11.8e \n",p2,h2,T_phmass(p2,h2),
                                                                            p2,s2,T_psmass(p2,s2));
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f     %3.1f      %11.8e \n",p3,h3,T_phmass(p3,h3),
                                                                            p3,s3,T_psmass(p3,s3));
    printf("_______________________________________________________________________\n\n\n");

    p1  = 0.001; p2 = 3; p3 = 3; h1 = 3000; h2 = 3000; h3 = 4000;
    double p1s = 0.1, p2s = 0.1, p3s = 2.5; s1 = 7.5, s2 = 8, s3 = 8;
    printf("_______________________________________________________________________\n");
    printf("                           Region 2a, 2b, 2c                           \n");
    printf("_______________________________________________________________________\n");
    printf("              Table 24             |               Table 29            \n");
    printf("___________________________________|___________________________________\n");
    printf(" p/MPa  h/(kJ/kg)        T/K       | p/MPa s/(kJ/kg-K)       T/K       \n");
    printf("___________________________________|___________________________________\n");
    printf("  %5.3f   %4.0f     %11.8e |  %3.1f     %3.1f      %11.8e \n",p1,h1,T_phmass(p1,h1),
                                                                          p1s,s1,T_psmass(p1s,s1));
    printf("  %3.0f     %4.0f     %11.8e |  %3.1f     %3.1f      %11.8e \n",p2,h2,T_phmass(p2,h2),
                                                                            p2s,s2,T_psmass(p2s,s2));
    printf("  %3.0f     %4.0f     %11.8e |  %3.1f     %3.1f      %11.8e \n",p3,h3,T_phmass(p3,h3),
                                                                            p3s,s3,T_psmass(p3s,s3));
    printf("___________________________________|___________________________________\n");
    p1  = 5; p2  = 5; p3  = 25; h1 = 3500; h2 = 4000; h3 = 3500;
    p1s = 8; p2s = 8; p3s = 90; s1 = 6;    s2 = 7.5;  s3 = 6;
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f     %3.1f      %11.8e \n",p1,h1,T_phmass(p1,h1),
                                                                            p1s,s1,T_psmass(p1s,s1));
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f     %3.1f      %11.8e \n",p2,h2,T_phmass(p2,h2),
                                                                            p2s,s2,T_psmass(p2s,s2));
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f     %3.1f      %11.8e \n",p3,h3,T_phmass(p3,h3),
                                                                            p3s,s3,T_psmass(p3s,s3));
    printf("_______________________________________________________________________\n\n");
    p1  = 40; p2  = 60; p3  = 60; h1 = 2700; h2 = 2700; h3 = 3200;
    p1s = 20; p2s = 80; p3s = 80; s1 = 5.75; s2 = 5.25; s3 = 5.75;
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f     %4.2f     %11.8e \n",p1,h1,T_phmass(p1,h1),
                                                                          p1s,s1,T_psmass(p1s,s1));
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f     %4.2f     %11.8e \n",p2,h2,T_phmass(p2,h2),
                                                                          p2s,s2,T_psmass(p2s,s2));
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f     %4.2f     %11.8e \n",p3,h3,T_phmass(p3,h3),
                                                                          p3s,s3,T_psmass(p3s,s3));
    printf("_______________________________________________________________________\n\n\n");

    printf("_______________________________________________________________________\n");
    printf("                             Region 3a, 3b                             \n");
    printf("_______________________________________________________________________\n");
    printf("              Table 5*             |               Table 12*           \n");
    printf("___________________________________|___________________________________\n");
    printf(" p/MPa  h/(kJ/kg)        T/K       | p/MPa s/(kJ/kg-K)       T/K       \n");
    printf("___________________________________|___________________________________\n");
    p1  = 20; p2  = 50; p3  = 100; h1 = 1700; h2 = 2000; h3 = 2100;
                                         s1 = 3.8;  s2 = 3.6;  s3 = 4.0;
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f      %3.1f     %11.8e \n",p1,h1,T_phmass(p1,h1),
                                                                          p1,s1,T_psmass(p1,s1));
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f      %3.1f     %11.8e \n",p2,h2,T_phmass(p2,h2),
                                                                          p2,s2,T_psmass(p2,s2));
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f      %3.1f     %11.8e \n",p3,h3,T_phmass(p3,h3),
                                                                          p3,s3,T_psmass(p3,s3));
    printf("_______________________________________________________________________\n\n");
    h1 = 2500; h2 = 2400; h3 = 2700;
    s1 = 5.0;  s2 = 4.5;  s3 = 5.0;
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f      %3.1f     %11.8e \n",p1,h1,T_phmass(p1,h1),
                                                                          p1,s1,T_psmass(p1,s1));
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f      %3.1f     %11.8e \n",p2,h2,T_phmass(p2,h2),
                                                                          p2,s2,T_psmass(p2,s2));
    printf("  %3.0f     %4.0f     %11.8e |  %3.0f      %3.1f     %11.8e \n",p3,h3,T_phmass(p3,h3),
                                                                          p3,s3,T_psmass(p3,s3));
    printf("_______________________________________________________________________\n");
    printf("* The Region 3a, 3b formulation comes from the 2014 \"Revised \n");
    printf("  Supplementary Release on Backward Equations for the Functions\n");
    printf("  T(p,h), v(p,h) and T(p,s), v(p,s) for Region 3 [IAPWS SR3-03(2014)]\" \n\n\n");

    printf("**************** Reverse Functions p(h,s) & T(h,s) ********************\n");
    printf(" Verification tables taken from the \"Revised Supplementary Release on \n");
    printf(" Backward Equations for Pressure as a Function of Enthalpy and Entropy \n");
    printf(" p(h,s) for Regions 1 and 2 of the IAPWS IF97\" [IAPWS SR2-01(2014)]   \n");
    printf("***********************************************************************\n\n");

    h1  = 0.001; h2 = 90; h3 = 1500;
    s1  = 0;       s2 =    0; s3 =  3.4;
    printf("_________________________________________________\n");
    printf("                     Region 1                    \n");
    printf("_________________________________________________\n");
    printf("                      Table 3                    \n");
    printf("_________________________________________________\n");
    printf("     h/(kJ/kg)     s/(kJ/kg-K)       P/MPa       \n");
    printf("_________________________________________________\n");
    printf("        %5.3f         %3.1f       %11.9e \n",h1,s1,p_hsmass(h1,s1));
    printf("        %5.0f         %3.1f       %11.9e \n",h2,s2,p_hsmass(h2,s2));
    printf("        %5.0f         %3.1f       %11.9e \n",h3,s3,p_hsmass(h3,s3));
    printf("_________________________________________________\n\n\n");


    printf("_________________________________________________\n");
    printf("                 Region 2a, 2b, 2c               \n");
    printf("_________________________________________________\n");
    printf("                      Table 9                    \n");
    printf("_________________________________________________\n");
    printf("     h/(kJ/kg)     s/(kJ/kg-K)       P/MPa       \n");
    printf("_________________________________________________\n");
    h1  = 2800; h2 = 2800; h3 = 4100;
    s1  =  6.5; s2 =  9.5; s3 =  9.5;
    printf("        %5.0f         %3.1f       %11.9e \n",h1,s1,p_hsmass(h1,s1));
    printf("        %5.0f         %3.1f       %11.9e \n",h2,s2,p_hsmass(h2,s2));
    printf("        %5.0f         %3.1f       %11.9e \n",h3,s3,p_hsmass(h3,s3));
    printf("_________________________________________________\n");
    h1  = 2800; h2 = 3600; h3 = 3600;
    s1  =  6.0; s2 =  6.0; s3 =  7.0;
    printf("        %5.0f         %3.1f       %11.9e \n",h1,s1,p_hsmass(h1,s1));
    printf("        %5.0f         %3.1f       %11.9e \n",h2,s2,p_hsmass(h2,s2));
    printf("        %5.0f         %3.1f       %11.9e \n",h3,s3,p_hsmass(h3,s3));
    printf("_________________________________________________\n");
    h1  = 2800; h2 = 2800; h3 = 3400;
    s1  =  5.1; s2 =  5.8; s3 =  5.8;
    printf("        %5.0f         %3.1f       %11.9e \n",h1,s1,p_hsmass(h1,s1));
    printf("        %5.0f         %3.1f       %11.9e \n",h2,s2,p_hsmass(h2,s2));
    printf("        %5.0f         %3.1f       %11.9e \n",h3,s3,p_hsmass(h3,s3));
    printf("_________________________________________________\n\n\n");


    printf("_________________________________________________\n");
    printf("                   Region 3a, 3b                 \n");
    printf("_________________________________________________\n");
    printf("                      Table 5*                   \n");
    printf("_________________________________________________\n");
    printf("     h/(kJ/kg)     s/(kJ/kg-K)       P/MPa       \n");
    printf("_________________________________________________\n");
    h1  = 1700; h2 = 2000; h3 = 2100;
    s1  =  3.8; s2 =  4.2; s3 =  4.3;
    printf("        %5.0f         %3.1f       %11.9e \n",h1,s1,p_hsmass(h1,s1));
    printf("        %5.0f         %3.1f       %11.9e \n",h2,s2,p_hsmass(h2,s2));
    printf("        %5.0f         %3.1f       %11.9e \n",h3,s3,p_hsmass(h3,s3));
    printf("_________________________________________________\n");
    h1  = 2600; h2 = 2400; h3 = 2700;
    s1  =  5.1; s2 =  4.7; s3 =  5.0;
    printf("        %5.0f         %3.1f       %11.9e \n",h1,s1,p_hsmass(h1,s1));
    printf("        %5.0f         %3.1f       %11.9e \n",h2,s2,p_hsmass(h2,s2));
    printf("        %5.0f         %3.1f       %11.9e \n",h3,s3,p_hsmass(h3,s3));
    printf("_________________________________________________\n");
    printf("* The Region 3a, 3b formulation comes from the 2014 \"Revised \n");
    printf("  Supplementary Release on Backward Equations p(h,s) for Region 3,\n");
    printf("  Equations as a Function of h and s for the Region Boundaries,\n");
    printf("  and and Equation Tsat(h,s) for Region 4 of the IAPWS IF97...\" \n");
    printf("  [IAPWS SR4-04(2014)]\" \n\n\n");
    
    printf("_________________________________________________\n");
    printf("    Region 4 (2-phase Saturation Temperature)    \n");
    printf("_________________________________________________\n");
    printf("                      Table 29*                  \n");
    printf("_________________________________________________\n");
    printf("     h/(kJ/kg)     s/(kJ/kg-K)        T/K        \n");
    printf("_________________________________________________\n");
    h1  = 1800; h2 = 2400; h3 = 2500;
    s1  =  5.3; s2 =  6.0; s3 =  5.5;
    printf("        %5.0f         %3.1f       %11.9e \n",h1,s1,T_hsmass(h1,s1));
    printf("        %5.0f         %3.1f       %11.9e \n",h2,s2,T_hsmass(h2,s2));
    printf("        %5.0f         %3.1f       %11.9e \n",h3,s3,T_hsmass(h3,s3));
    printf("_________________________________________________\n");
    printf("* The Region 3a, 3b formulation comes from the 2014 \"Revised \n");
    printf("  Supplementary Release on Backward Equations p(h,s) for Region 3,\n");
    printf("  Equations as a Function of h and s for the Region Boundaries,\n");
    printf("  and and Equation Tsat(h,s) for Region 4 of the IAPWS IF97...\" \n");
    printf("  [IAPWS SR4-04(2014)]\" \n\n\n");

    return 0;
}