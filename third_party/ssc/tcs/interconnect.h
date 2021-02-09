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

#ifndef __interconnect_
#define __interconnect_

#include <string>
#include <vector>
#include "htf_props.h"


enum class CpntType {
    Fitting,
    Pipe,
    Flex_Hose,
    FINAL_ENTRY,
};

double FrictionFactor_FlexHose(double Re, double D);

struct IntcOutputs
{
    double heat_loss;               // [W]
    double temp_drop;               // [K]
    double temp_out;                // [K]
    double temp_ave;                // [K]
    double pressure_drop;           // [Pa]
    double pressure_out;            // [Pa]
    double pressure_ave;            // [Pa]
    double internal_energy;         // [J]

    IntcOutputs();
};

class intc_cpnt
{
private:
    double k_;                       // minor loss coefficient [-]
    double d_in_;                    // inner diameter [m]
    double l_;                       // length [m]
    double rough_;                   // roughness (inside) [m]
    double hl_coef_;                 // overall heat loss coefficient [W/(m2-K)]
    double mc_;                      // heat capacity w/o htf [J/K]
    double wall_thick_;              // wall thickness [m]
    CpntType Type;
    bool OuterSurfArea_valid_;
    double OuterSurfArea_;
    bool FlowArea_valid_;
    double FlowArea_;                // cross-sectional area for flow [m^2]
    bool FluidVolume_valid_;
    double FluidVolume_;

    void calcOuterSurfArea();
    void calcFlowArea();
    void calcFluidVolume();

public:
    intc_cpnt(double k = 0, double d = 0, double l = 0, double rough = 0, double u = 0,
        double mc = 0, CpntType type = CpntType::Fitting);
    ~intc_cpnt();

    double getK() const;
    void setK(double k);
    double getD() const;
    void setD(double d);
    double getLength() const;
    void setLength(double l);
    double getRelRough() const;
    void setRelRough(double rough);
    double getHLCoef() const;
    void setHLCoef(double u);
    double getHeatCap() const;
    void setHeatCap(double mc);
    double getWallThick() const;
    void setWallThick(double wall_thick);
    CpntType getType() const;
    double getOuterSurfArea();
    double getFlowArea();
    double getFluidVolume();

    double HeatLoss(double T_cpnt, double T_db);
    double TempDrop(HTFProperties *fluidProps, double m_dot, double T_in, double heatLoss);
    double TempDrop(HTFProperties *fluidProps, double m_dot, double T_in, double T_cpnt, double T_db);
    double PressureDrop(HTFProperties *fluidProps, double m_dot, double T_htf_ave, double P_htf_ave);
    double InternalEnergy(HTFProperties *fluidProps, double T_cpnt, double T_htf, double P_htf_ave);
    IntcOutputs State(HTFProperties *fluidProps, double m_dot, double T_in, double T_cpnt, double T_db, double P_htf_ave);
};

class interconnect
{
private:
    std::vector<intc_cpnt> cpnts;
    int N_cpnts_;
    HTFProperties *FluidProps_;
    bool Length_valid_;
    double l_;
    bool HeatCap_valid_;
    double mc_;
    bool OuterSurfArea_valid_;
    double OuterSurfArea_;
    bool FluidVolume_valid_;
    double FluidVolume_;

    void calcLength();
    void calcHeatCap();
    void calcOuterSurfArea();
    void calcFluidVolume();
public:
    interconnect();
    interconnect(HTFProperties *fluidProps, double *k, double *d, double *l, double *rough, double *u, double *mc,
        double *type, int n_cpnts);
    ~interconnect();

    void import_cpnts(double *k, double *d, double *l, double *rough, double *u, double *mc, double *type, int num_cpnts);
    void resetValues();

    void setFluidProps(HTFProperties *fluidProps);
    int getNcpnts();
    double getK(std::size_t cpnt) const;
    double getD(std::size_t cpnt) const;
    double getLength();
    double getLength(std::size_t cpnt) const;
    double getRelRough(std::size_t cpnt) const;
    double getHLCoef(std::size_t cpnt) const;
    double getHeatCap();
    double getHeatCap(std::size_t cpnt) const;
    CpntType getType(std::size_t cpnt) const;
    double getOuterSurfArea();
    double getOuterSurfArea(std::size_t cpnt);
    double getFlowArea(std::size_t cpnt);
    double getFluidVolume();
    double getFluidVolume(std::size_t cpnt);

    IntcOutputs State(double m_dot, double T_in, double T_db, double P_in);
};

#endif
