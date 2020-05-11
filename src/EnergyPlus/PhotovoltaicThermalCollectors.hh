// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#ifndef PhotovoltaicThermalCollectors_hh_INCLUDED
#define PhotovoltaicThermalCollectors_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/PlantComponent.hh>

namespace EnergyPlus {

namespace PhotovoltaicThermalCollectors {

    enum struct WorkingFluidEnum
    {
        LIQUID,
        AIR
    };

    enum struct ThermEfficEnum
    {
        SCHEDULED,
        FIXED
    };

    extern int NumPVT; // number of transpired collectors in model

    struct SimplePVTModelStruct
    {
        // Members
        std::string Name;
        Real64 ThermalActiveFract;     // fraction of surface area with active thermal collection
        ThermEfficEnum ThermEfficMode; // setting for how therm effic is determined
        Real64 ThermEffic;             // fixed or current Therm efficiency
        int ThermEffSchedNum;          // pointer to schedule for therm effic (if any)
        Real64 SurfEmissivity;         // surface emittance in long wave IR
        Real64 LastCollectorTemp;      // store previous temperature
        Real64 CollectorTemp;          // average solar collector temp.

        // Default Constructor
        SimplePVTModelStruct()
            : ThermalActiveFract(0.0), ThermEfficMode(ThermEfficEnum::FIXED), ThermEffic(0.0), ThermEffSchedNum(0), SurfEmissivity(0.0),
              LastCollectorTemp(0.0), CollectorTemp(0.0)
        {
        }
    };

    struct BIPVTModelStruct
    {

        // Members
        std::string Name;
        std::string OSCMName;         // OtherSideConditionsModel
        int OSCMPtr;                  // OtherSideConditionsModel index
        int SchedPtr;                 // Availablity schedule
        Real64 PVEffGapWidth;         // Effective Gap Plenum Behind PV modules (m)
        Real64 EffCollHeight;         // Effective Overall Width of Collector (m)
        Real64 EffCollWidth;          // Effective Overall Height of Collector (m)
        Real64 PVTranAbsProduct;      // PV Transmittance-Absorptance Product
        Real64 BackMatTranAbsProduct; // Backing Material Normal Transmittance-Absorptance Product
        Real64 PVAreaFract;           // Fraction of collector gross area covered by PV cells
        Real64 PVRTop;                // PV module top thermal resistance (m2-DegC/W)
        Real64 PVRBot;                // PV module bottom thermal resistance (m2-DegC/W)
        Real64 PVGEmiss;              // Emissivity PV modules
        Real64 BackMatEmiss;          // Emissivity of backing material
        Real64 LastCollectorTemp;     // store previous temperature (DegC)
        Real64 CollectorTemp;         // average solar collector temp (DegC)
        Real64 Tplen;                 // modeled drybulb temperature for air through BIPVT channel (DegC)
        Real64 Tcoll;                 // modeled temperature of BIPVT channel surface on PV side (DegC)
        Real64 HrPlen;                // Modeled radiation coef for OSCM (W/m2-C)
        Real64 HcPlen;               // Modeled Convection coef for OSCM (W/m2-C)

        // Default Constructor
        BIPVTModelStruct()
            : OSCMPtr(0), SchedPtr(0), PVEffGapWidth(0.0), EffCollHeight(0.0), EffCollWidth(0.0), PVTranAbsProduct(0.0), BackMatTranAbsProduct(0.0),
            PVAreaFract(0.0), PVRTop(0.0), PVRBot(0.0), PVGEmiss(0.0), BackMatEmiss(0.0), LastCollectorTemp(0.0), CollectorTemp(0.0), Tplen(20.0),
            Tcoll(20.0), HrPlen(1.0), HcPlen(10.0)
        {
        }
    };

    struct PVTReportStruct
    {
        // Members
        Real64 ThermEfficiency;  // Thermal efficiency of solar energy conversion
        Real64 ThermPower;       // Heat gain or loss to collector fluid (W)
        Real64 ThermHeatGain;    // Heat gain to collector fluid (W)
        Real64 ThermHeatLoss;    // Heat loss from collector fluid (W)
        Real64 ThermEnergy;      // Energy gained (or lost) to collector fluid (J)
        Real64 MdotWorkFluid;    // working fluid mass flow rate (kg/s)
        Real64 TinletWorkFluid;  // working fluid inlet temp (C)
        Real64 ToutletWorkFluid; // working fluid outlet temp (C)
        Real64 BypassStatus;     // 0 = no bypass, 1=full bypass

        // Default Constructor
        PVTReportStruct()
            : ThermEfficiency(0.0), ThermPower(0.0), ThermHeatGain(0.0), ThermHeatLoss(0.0), ThermEnergy(0.0), MdotWorkFluid(0.0),
              TinletWorkFluid(0.0), ToutletWorkFluid(0.0), BypassStatus(0.0)
        {
        }
    };

    struct PVTCollectorStruct : PlantComponent
    {
        // Members
        std::string Name;            // Name of PVT collector
        int TypeNum;                 // Plant Side Connection: 'TypeOf_Num' assigned in DataPlant
        int WLoopNum;                // Water plant loop index number
        int WLoopSideNum;            // Water plant loop side index
        int WLoopBranchNum;          // Water plant loop branch index
        int WLoopCompNum;            // Water plant loop component index
        bool EnvrnInit;              // manage begin environment inits
        bool SizingInit;             // manage when sizing is complete
        std::string PVTModelName;    // Name of PVT performance object
        int PVTModelType;            // model type indicator, only simple avail now
        int SurfNum;                 // surface index
        std::string PVname;          // named Generator:Photovoltaic object
        int PVnum;                   // PV index
        bool PVfound;                // init, need to delay get input until PV gotten
        SimplePVTModelStruct Simple; // Simple performance data structure.
        BIPVTModelStruct BIPVT;      // BIPVT performance data structure.
        Real64 QdotSource;           // Source/sink term
        WorkingFluidEnum WorkingFluidType;
        int PlantInletNodeNum;
        int PlantOutletNodeNum;
        int HVACInletNodeNum;
        int HVACOutletNodeNum;
        Real64 DesignVolFlowRate;
        bool DesignVolFlowRateWasAutoSized; // true if design volume flow rate was autosize on input
        Real64 MaxMassFlowRate;
        Real64 MassFlowRate;
        Real64 AreaCol;
        bool BypassDamperOff;
        bool CoolingUseful;
        bool HeatingUseful;
        PVTReportStruct Report;
        bool MySetPointCheckFlag;
        bool MyOneTimeFlag;
        bool SetLoopIndexFlag;

        // Default Constructor
        PVTCollectorStruct()
            : TypeNum(0), WLoopNum(0), WLoopSideNum(0), WLoopBranchNum(0), WLoopCompNum(0), EnvrnInit(true), SizingInit(true), PVTModelType(0),
              SurfNum(0), PVnum(0), PVfound(false), WorkingFluidType(WorkingFluidEnum::LIQUID), PlantInletNodeNum(0), PlantOutletNodeNum(0),
              HVACInletNodeNum(0), HVACOutletNodeNum(0), DesignVolFlowRate(0.0), DesignVolFlowRateWasAutoSized(false), MaxMassFlowRate(0.0),
              MassFlowRate(0.0), AreaCol(0.0), BypassDamperOff(true), CoolingUseful(false), HeatingUseful(false), MySetPointCheckFlag(true),
              MyOneTimeFlag(true), SetLoopIndexFlag(true), QdotSource(0.0)
        {
        }

        static PlantComponent *factory(std::string const &objectName);

        void onInitLoopEquip(EnergyPlusData &EP_UNUSED(state), const PlantLocation &calledFromLocation) override;

        void simulate(EnergyPlusData &EP_UNUSED(state), const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag) override;

        void setupReportVars();

        void initialize(bool FirstHVACIteration);

        void size();

        void control();

        void calculate();

       void SimplePVTcalculate();

       void BIPVTcalculate();

       void BIPVT_MaxHeatGain_calculate(Real64 tsp, std::string Mode, Real64 &bfr, Real64 &q, Real64 &tmixed, Real64 &ThEff, Real64 &tpv);

       void solve_lin_sys_back_sub(Real64 jj[9], Real64 f[3], Real64 (&y)[3]);

       void update();
    };

    extern Array1D<PVTCollectorStruct> PVT;

    void clear_state();

    void GetPVTcollectorsInput();

    void GetPVTSimpleCollectorsInput(int NumSimplePVTPerform, Array1D<SimplePVTModelStruct> &tmpSimplePVTperf);

    void GetBIPVTCollectorsInput(int NumBIPVTPerform, Array1D<BIPVTModelStruct> &tmpBIPVTperf);

    void GetMainPVTInput(int NumPVT, Array1D<PVTCollectorStruct> &PVT, Array1D<SimplePVTModelStruct> tmpSimplePVTperf, Array1D<BIPVTModelStruct> tmpBIPVTperf);

    void simPVTfromOASys(EnergyPlusData &state, int index, bool FirstHVACIteration);

    int getPVTindexFromName(std::string const &name);

    void GetPVTThermalPowerProduction(int PVindex, Real64 &ThermalPower, Real64 &ThermalEnergy);

    int GetAirInletNodeNum(std::string const &PVTName, bool &ErrorsFound);

    int GetAirOutletNodeNum(std::string const &PVTName, bool &ErrorsFound);

    void GetPVTmodelIndex(int const SurfacePtr, int &PVTIndex);

    void SetPVTQdotSource(int const PVTNum, Real64 const QSource);

    void GetPVTTsColl(int const PVTNum, Real64 &TsColl);

} // namespace PhotovoltaicThermalCollectors

} // namespace EnergyPlus

#endif
