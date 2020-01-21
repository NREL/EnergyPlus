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

#ifndef GroundHeatExchangers_hh_INCLUDED
#define GroundHeatExchangers_hh_INCLUDED

// C++ Headers
#include <deque>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// JSON Headers
#include <../third_party/nlohmann/json.hpp>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <EnergyPlus/PlantComponent.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>

namespace EnergyPlus {

namespace GroundHeatExchangers {

    struct ThermoProps
    {
        // members
        Real64 k;           // Thermal conductivity [W/m-K]
        Real64 rho;         // Density [kg/m3]
        Real64 cp;          // Specific heat [J/kg-K]
        Real64 rhoCp;       // Heat capacity [J/m3-K]
        Real64 diffusivity; // Thermal diffusivity [m2/s]

        // default constructor
        ThermoProps() : k(0.0), rho(0.0), cp(0.0), rhoCp(0.0), diffusivity(0.0) {
        }

        // member constructor
        ThermoProps(Real64 const k, Real64 const rho, Real64 const cp)
            : k(k), rho(rho), cp(cp)
        {
            this->setup();
        }

        // default destructor
        ~ThermoProps() = default;

        // member methods
        void setup();
    };

    struct FluidWorker
    {
        // members
        int loopNum;
        std::string fluidName;
        int fluidIdx;

        // default constructor
        FluidWorker() : loopNum(0), fluidIdx(0)
        {
        }

        // default destructor
        ~FluidWorker() = default;

        // member methods
        void setup(int loopNum);
        Real64 getSpHt(Real64 const &temperature, const std::string &routineName);
        Real64 getCond(Real64 const &temperature, const std::string &routineName);
        Real64 getVisc(Real64 const &temperature, const std::string &routineName);
        Real64 getDens(Real64 const &temperature, const std::string &routineName);
        Real64 getPrtl(Real64 const &temperature, const std::string &routineName);
    };

    struct Pipe : ThermoProps, FluidWorker
    {
        // members
        Real64 outDia;        // Outer diameter [m]
        Real64 innerDia;      // Inner diameter [m]
        Real64 length;        // Length [m]
        Real64 outRadius;     // Outer radius [m]
        Real64 innerRadius;   // Inner radius [m]
        Real64 wallThickness; // Pipe wall thickness [m]
        Real64 areaCrOuter;   // Outer cross-sectional area [m2]
        Real64 areaCrInner;   // Inner cross-sectional area [m2]
        Real64 areaCrPipe;    // Pipe wall cross-sectional area [m2]
        Real64 areaSurfOuter; // Pipe outer surface area [m2]
        Real64 areaSurfInner; // Pipe inner surface area [m2]
        Real64 volTotal;      // Total pipe volume [m3]
        Real64 volFluid;      // Fluid volume [m3]
        Real64 volPipeWall;   // Pipe wall volume [m3]
        Real64 friction;      // Friction factor [-]
        Real64 resistPipe;    // Total pipe resistance [K/(W/m)]
        Real64 resistConv;    // Pipe convection resistance [K/(W/m)]
        static constexpr int numCells = 16;    // Number of pipe elements
        std::vector<Real64> cellTemps; // Pipe temperature for each node
        std::deque<Real64> inletTemps;                                              // Inlet temperature history [C]
        std::deque<Real64> inletTempTimes;                                           // Times for respective inlet temperatures [s]
        Real64 outletTemp;                                                             // Pipe outlet temperature [C]
        bool applyTransitDelay;

        // default constructor
        Pipe() : outDia(0.0), innerDia(0.0), length(0.0), outRadius(0.0), innerRadius(0.0), wallThickness(0.0),
                 areaCrOuter(0.0), areaCrInner(0.0), areaCrPipe(0.0), areaSurfOuter(0.0), areaSurfInner(0.0), volTotal(0.0),
                 volFluid(0.0), volPipeWall(0.0), friction(0.0), resistPipe(0.0), resistConv(0.0), cellTemps(numCells, 0.0),
                 inletTemps({0.0}), inletTempTimes({0.0}), outletTemp(0.0), applyTransitDelay(true)
        {
        }

        // default destructor
        ~Pipe() = default;

        // members methods
        void setup(Real64 const &initTemp);
        Real64 calcTransitTime(Real64 flowRate, Real64 temperature);
        void simulate(Real64 time, Real64 timeStep, Real64 flowRate, Real64 inletTemp);
        Real64 plugFlowOutletTemp(Real64 time);
        void logInletTemps(Real64 inletTemp, Real64 time);
        Real64 mdotToRe(Real64 flowRate, Real64 temperature);
        Real64 calcFrictionFactor(Real64 Re);
        Real64 calcConductionResistance();
        Real64 calcConvectionResistance(Real64 flowRate, Real64 temperature);
        Real64 calcResistance(Real64 flowRate, Real64 temperature);
        Real64 turbulentNusselt(Real64 Re, Real64 temperature);
        static Real64 laminarNusselt();
        static Real64 laminarFrictionFactor(Real64 Re);
        static Real64 turbulentFrictionFactor(Real64 Re);
    };

    struct BHSegmentPassThrough
    {
        // members
        Real64 temperature;

        // default constructor
        BHSegmentPassThrough() : temperature(0.0)
        {
        }

        // default destructor
        ~BHSegmentPassThrough() = default;

        Real64 getOutletTemp2();

        void simulate(Real64 const &temp);
    };

    struct BHSegment {

        // members
        static constexpr int numPipes = 2;
        Pipe pipe;
        ThermoProps soil;
        ThermoProps grout;
        Real64 diameter;
        Real64 length;
        Real64 groutVolume;
        static constexpr int numEquations = 5;
        std::vector<Real64> temps;
        static constexpr Real64 groutFrac = 0.5;
        Real64 bhResist;
        Real64 dcResist;
        Real64 boundaryTemp;
        Real64 heatRate;
        Real64 outletTemp1;
        Real64 outletTemp2;

        // default constructor
        BHSegment() : diameter(0.0), length(0.0), groutVolume(0.0), temps(numEquations, 0.0),
                      bhResist(0.0), dcResist(0.0), boundaryTemp(0.0), heatRate(0.0), outletTemp1(0.0), outletTemp2(0.0)
        {
        }

        // default destructor
        ~BHSegment() = default;

        // member methods
        void setup(Real64 const &initTemp, int const &loopNum);
        Real64 calcGroutVolume();
        Real64 calcTotalPipeVolume();
        Real64 calcSegVolume();
        Real64 getOutletTemp1();
        Real64 getOutletTemp2();
        Real64 getHeatRate();
        std::vector<Real64> rhs(std::vector<Real64> const &y, std::vector<Real64> const &params);
        std::vector<Real64> rk4(std::vector<Real64> const &y, Real64 const &h, std::vector<Real64> const &params);
        void simulate(Real64 const &inletTemp1,
                      Real64 const &inletTemp2,
                      Real64 const &flowRate);
    };

    struct Interp1D
    {
        std::vector<Real64> x_data;
        std::vector<Real64> y_data;
        std::string routineName;
        std::vector<std::pair<Real64, Real64> > table;
        bool extrapolate = false;

        // constructor
        Interp1D(std::vector<Real64> &x_data, std::vector<Real64> &y_data,
                 std::string &routineName, bool extrapolate = false) {

            this->x_data = x_data;
            this->y_data = y_data;
            this->routineName = routineName;
            this->extrapolate = extrapolate;

            if (this->x_data.size() == this->y_data.size()) {
                for (std::size_t i = 0; i != this->x_data.size(); ++i) {
                    this->table.emplace_back(std::pair<Real64, Real64> {this->x_data[i], this->y_data[i]});
                }
            } else {
                ShowFatalError(routineName + ": Number of X and Y data must be equal.");
            }
            // add option later to ask if the data needs to be sorted
            // std::sort(table.begin(), table.end());
        }

        // default constructor
        Interp1D() = default;

        // destructor
        ~Interp1D() = default;

        // member functions
        Real64 interpolate(Real64 &x);
    };

    struct BaseAgg
    {
        // member variables
        Real64 ts;  // GHE time scale
        Interp1D g_data;  // g-function data
        std::vector<Real64> energy;  // energy history
        std::vector<Real64> dts;  // time steps
        Real64 prev_update_time;  // previous update time

        // constructor
        BaseAgg() : ts(0.0), prev_update_time(0.0)
        {};

        // destructor
        ~BaseAgg() = default;

        // member functions
        static std::vector<Real64> calc_times(std::vector<Real64> &times)
        {
            std::vector<Real64> v = times;
            std::reverse(std::begin(v), std::end(v));
            std::vector<Real64> sums (v.size());
            std::partial_sum(std::begin(v), std::end(v), sums.begin());
            std::reverse(std::begin(sums), std::end(sums));
            return sums;
        }

        // virtual functions
        virtual void aggregate(Real64 &time, Real64 &energy) = 0;
        virtual Real64 calc_temporal_superposition(Real64 &timeStep, Real64 & flowRate) = 0;
        virtual Real64 get_g_value(Real64 &time) = 0;
        virtual Real64 get_q_prev() = 0;

    };

    struct SubHourAgg : BaseAgg
    {
        std::string routineName = "Subhourly Aggregation";
        Real64 subHrEnergy = 0.0;

        // constructor
        explicit SubHourAgg(const nlohmann::json &j)
        {
            this->energy.emplace_back(0.0);
            this->dts.emplace_back(DataGlobals::SecInHour);
            this->ts = j["time-scale"];
            std::vector<Real64> lntts = j["g-function-data"]["lntts"];
            std::vector<Real64> g = j["g-function-data"]["g"];
            this->g_data = Interp1D(lntts, g, routineName, true);
        };

        // destructor
        ~SubHourAgg() = default;

        // member functions
        void aggregate(Real64 &time, Real64 &energy) override;
        Real64 calc_temporal_superposition(Real64 &timeStep, Real64 & flowRate) override;
        Real64 get_g_value(Real64 &time) override;
        Real64 get_q_prev() override;
    };

    struct PipeProps : ThermoProps
    {
        // Members
        Real64 outDia;      // Outer diameter of the pipe [m]
        Real64 innerDia;    // Inner diameter of the pipe [m]
        Real64 outRadius;   // Outer radius of the pipe [m]
        Real64 innerRadius; // Inner radius of the pipe [m]
        Real64 thickness;   // Thickness of the pipe wall [m]

        // Default constructor
        PipeProps() : outDia(0.0), innerDia(0.0), outRadius(0.0), innerRadius(0.0), thickness(0.0)
        {
        }

        // Default destructor
        ~PipeProps() = default;
    };

    struct BHPropsStruct
    {
        // Members
        std::string name;                 // Name
        Real64 topDepth;                // Depth of top of borehole {m}
        Real64 length;                  // Length of borehole from top of borehole {m}
        Real64 diameter;                // Diameter of borehole {m}
        ThermoProps grout; // Grout properties
        PipeProps pipe;             // Pipe properties
        Real64 shankSpace;               // U-tube, shank-to-shank spacing {m}

        // Default constructor
        BHPropsStruct() : topDepth(0.0), length(0.0), diameter(0.0), shankSpace(0.0)
        {
        }

        // Default destructor
        ~BHPropsStruct() = default;
    };

    struct MyCartesian
    {
        // Members
        Real64 x;
        Real64 y;
        Real64 z;

        // Default constructor
        MyCartesian() : x(0.0), y(0.0), z(0.0)
        {
        }

        // Default destructor
        ~MyCartesian() = default;
    };

    struct BHStruct
    {
        // Members
        std::string name;                           // Name
        Real64 xLoc;                                // X-direction location {m}
        Real64 yLoc;                                // Y-direction location {m}
        Real64 dl_i;                                // length between points
        Real64 dl_ii;                               // length between points
        Real64 dl_j;                                // length between points
        std::shared_ptr<BHPropsStruct> props;      // Properties
        std::vector<MyCartesian> pointLocations_i;  // Point locations for when computing temperature response of other boreholes on this bh
        std::vector<MyCartesian> pointLocations_ii; // Point locations for when computing temperature response of this bh on itself
        std::vector<MyCartesian> pointLocations_j;  // Point locations for when other bh are computing the temperature response of this bh on themselves

        // Default constructor
        BHStruct() : xLoc(0.0), yLoc(0.0), dl_i(0.0), dl_ii(0.0), dl_j(0.0)
        {
        }

        // Default destructor
        ~BHStruct() = default;
    };

    struct GLHEVertArray
    {
        // Members
        std::string name;                           // Name
        int numBHinXDirection;                      // Number of boreholes in X direction
        int numBHinYDirection;                      // Number of boreholes in Y direction
        Real64 bhSpacing;                           // Borehole center-to-center spacing {m}
        std::shared_ptr<BHPropsStruct> props; // Properties

        // Default constructor
        GLHEVertArray() : numBHinXDirection(0), numBHinYDirection(0), bhSpacing(0.0)
        {
        }

        // Default destructor
        ~GLHEVertArray() = default;
    };

    struct GLHEResponseFactors
    {
        // Members
        std::string name;                                              // Name
        int numBoreholes;                                              // Number of boreholes
        int numGFuncPairs;                                             // Number of g-function pairs
        Real64 gRefRatio;                                              // Reference ratio of g-function set
        Real64 maxSimYears;                                            // Maximum length of simulation in years
        Array1D<Real64> time;                                          // response time in seconds
        Array1D<Real64> LNTTS;                                         // natural log of Non Dimensional Time Ln(t/ts)
        Array1D<Real64> GFNC;                                          // G-function ( Non Dimensional temperature response factors)
        std::shared_ptr<BHPropsStruct> props;                    // Properties
        std::vector<std::shared_ptr<BHStruct>> myBorholes; // Boreholes used by this response factors object

        // Default constructor
        GLHEResponseFactors() : numBoreholes(0), numGFuncPairs(0), gRefRatio(0.0), maxSimYears(0.0)
        {
        }

        // Default destructor
        ~GLHEResponseFactors() = default;
    };

    struct GLHEBase : PlantComponent, PlantLocation
    {
        // Members
        bool available;   // need an array of logical--load identifiers of available equipment
        bool on;          // simulate the machine at it's operating part load ratio
        std::string name; // user identifier
        int inletNodeNum;  // Node number on the inlet side of the plant
        int outletNodeNum; // Node number on the outlet side of the plant
        ThermoProps soil;
        PipeProps pipe;
        ThermoProps grout;
        std::shared_ptr<GLHEResponseFactors> myRespFactors;
        Real64 designFlow;            // Design volumetric flow rate			[m3/s]
        Real64 designMassFlow;        // Design mass flow rate				[kg/s]
        Real64 tempGround;            // The far field temperature of the ground   [degC]
        Array1D<Real64> QnMonthlyAgg; // Monthly aggregated normalized heat extraction/rejection rate [W/m]
        Array1D<Real64> QnHr;         // Hourly aggregated normalized heat extraction/rejection rate [W/m]
        Array1D<Real64> QnSubHr; // Contains the sub-hourly heat extraction/rejection rate normalized by the total active length of bore holes  [W/m]
        int prevHour;
        int AGG;               // Minimum Hourly History required
        int SubAGG;            // Minimum sub-hourly History
        Array1D_int LastHourN; // Stores the Previous hour's N for past hours until the minimum sub-hourly history
        Real64 bhTemp;         // [degC]
        Real64 massFlowRate;   // [kg/s]
        Real64 outletTemp;     // [degC]
        Real64 inletTemp;      // [degC]
        Real64 aveFluidTemp;   // [degC]
        Real64 QGLHE;          // [W] heat transfer rate
        bool myFlag;
        bool myEnvrnFlag;
        bool gFunctionsExist;
        Real64 lastQnSubHr;
        Real64 HXResistance;    // The thermal resistance of the GHX, (K per W/m)
        Real64 totalTubeLength; // The total length of pipe. NumBoreholes * BoreholeDepth OR Pi * Dcoil * NumCoils
        Real64 timeSS;          // Steady state time
        Real64 timeSSFactor;    // Steady state time factor for calculation
        std::shared_ptr<BaseGroundTempsModel> groundTempModel;

        // some statics pulled out into member variables
        bool firstTime;
        int numErrorCalls;
        Real64 ToutNew;
        int PrevN;             // The saved value of N at previous time step
        bool updateCurSimTime; // Used to reset the CurSimTime to reset after WarmupFlag
        bool triggerDesignDayReset;

        // Default constructor
        GLHEBase()
            : available(false), on(false), inletNodeNum(0), outletNodeNum(0), designFlow(0.0),
              designMassFlow(0.0), tempGround(0.0), prevHour(1), AGG(0), SubAGG(0), bhTemp(0.0), massFlowRate(0.0), outletTemp(0.0), inletTemp(0.0),
              aveFluidTemp(0.0), QGLHE(0.0), myFlag(true), myEnvrnFlag(true), gFunctionsExist(false), lastQnSubHr(0.0), HXResistance(0.0),
              totalTubeLength(0.0), timeSS(0.0), timeSSFactor(0.0), firstTime(true), numErrorCalls(0), ToutNew(19.375), PrevN(1),
              updateCurSimTime(true), triggerDesignDayReset(false)
        {
        }

        // Default destructor
        ~GLHEBase() = default;

        virtual void calcGFunctions() = 0;

        void calcAggregateLoad();

        void updateGHX();

        void calcGroundHeatExchanger();

        static bool isEven(int val);

        Real64 interpGFunc(Real64);

        void makeThisGLHECacheAndCompareWithFileCache();

        virtual void makeThisGLHECacheStruct() = 0;

        virtual void readCacheFileAndCompareWithThisGLHECache() = 0;

        void onInitLoopEquip(PlantLocation const &calledFromLocation) override;

        void simulate(PlantLocation const &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag) override;

        static PlantComponent *factory(int objectType, std::string const &objectName);

        virtual Real64 getGFunc(Real64) = 0;

        virtual void initGLHESimVars() = 0;

        virtual Real64 calcHXResistance() = 0;

        virtual void getAnnualTimeConstant() = 0;
    };

    struct GLHEVert : GLHEBase
    {
        // Members
        Real64 bhDiameter;  // Diameter of borehole {m}
        Real64 bhRadius;    // Radius of borehole {m}
        Real64 bhLength;    // Length of borehole {m}
        Real64 bhUTubeDist; // Distance between u-tube legs {m}

        // Parameters for the multipole method
        Real64 theta_1;
        Real64 theta_2;
        Real64 theta_3;
        Real64 sigma;

        nlohmann::json myCacheData;

        std::vector<Real64> GFNC_shortTimestep;
        std::vector<Real64> LNTTS_shortTimestep;

        // Default constructor
        GLHEVert() : bhDiameter(0.0), bhRadius(0.0), bhLength(0.0), bhUTubeDist(0.0), theta_1(0.0), theta_2(0.0), theta_3(0.0), sigma(0.0)
        {
        }

        // Default Destructor
        ~GLHEVert() = default;

        static std::vector<Real64> distances(MyCartesian const &point_i, MyCartesian const &point_j);

        Real64 calcResponse(std::vector<Real64> const &dists, Real64 const &currTime);

        Real64 integral(MyCartesian const &point_i, std::shared_ptr<BHStruct> const &bh_j, Real64 const &currTime);

        Real64
        doubleIntegral(std::shared_ptr<BHStruct> const &bh_i, std::shared_ptr<BHStruct> const &bh_j, Real64 const &currTime);

        void calcShortTimestepGFunctions();

        void calcLongTimestepGFunctions();

        void calcGFunctions() override;

        Real64 calcHXResistance() override;

        void initGLHESimVars() override;

        void getAnnualTimeConstant() override;

        Real64 getGFunc(Real64 time) override;

        void makeThisGLHECacheStruct() override;

        void readCacheFileAndCompareWithThisGLHECache() override;

        void writeGLHECacheToFile();

        Real64 calcBHAverageResistance();

        Real64 calcBHTotalInternalResistance();

        Real64 calcBHGroutResistance();

        Real64 calcPipeConductionResistance();

        Real64 calcPipeConvectionResistance();

        static Real64 frictionFactor(Real64 reynoldsNum);

        Real64 calcPipeResistance();

        void combineShortAndLongTimestepGFunctions();
    };

    struct EnhancedGHE : PlantComponent
    {
        // members
        std::string name;

        void setupOutputVars();

        void simulate(const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag) override;

        void getDesignCapacities(const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad) override;

        void getDesignTemperatures(Real64 &TempDesCondIn, Real64 &TempDesEvapOut) override;

        void getSizingFactor(Real64 &sizFac) override;

        void onInitLoopEquip(const PlantLocation &calledFromLocation) override;
    };

    struct GLHESlinky : GLHEBase
    {

        // Members
        bool verticalConfig;  // HX Configuration Flag
        Real64 coilDiameter;  // Diameter of the slinky coils [m]
        Real64 coilPitch;     // Center-to-center slinky coil spacing [m]
        Real64 coilDepth;     // Average depth of the coil [m]
        Real64 trenchDepth;   // Trench depth from ground surface to trench bottom [m]
        Real64 trenchLength;  // Length of single trench [m]
        int numTrenches;      // Number of parallel trenches [m]
        Real64 trenchSpacing; // Spacing between parallel trenches [m]
        int numCoils;         // Number of coils
        int monthOfMinSurfTemp;
        Real64 maxSimYears;
        Real64 minSurfTemp;
        Array1D<Real64> X0;
        Array1D<Real64> Y0;
        Real64 Z0;

        // Default constructor
        GLHESlinky()
            : verticalConfig(false), coilDiameter(0.0), coilPitch(0.0), coilDepth(0.0), trenchDepth(0.0), trenchLength(0.0), numTrenches(0),
              trenchSpacing(0.0), numCoils(0), monthOfMinSurfTemp(0), maxSimYears(0.0), minSurfTemp(0.0), Z0(0.0)
        {
        }

        // Default destructor
        ~GLHESlinky() = default;

        Real64 calcHXResistance() override;

        void calcGFunctions() override;

        void initGLHESimVars() override;

        void getAnnualTimeConstant() override;

        Real64 doubleIntegral(int m, int n, int m1, int n1, Real64 t, int I0, int J0);

        Real64 integral(int m, int n, int m1, int n1, Real64 t, Real64 eta, Real64 J0);

        Real64 distance(int m, int n, int m1, int n1, Real64 eta, Real64 theta);

        Real64 distanceToFictRing(int m, int n, int m1, int n1, Real64 eta, Real64 theta);

        Real64 distToCenter(int m, int n, int m1, int n1);

        Real64 nearFieldResponseFunction(int m, int n, int m1, int n1, Real64 eta, Real64 theta, Real64 t);

        Real64 midFieldResponseFunction(int m, int n, int m1, int n1, Real64 t);

        Real64 getGFunc(Real64 time) override;

        void makeThisGLHECacheStruct() override;

        void readCacheFileAndCompareWithThisGLHECache() override;
    };

    void clear_state();

    Real64 smoothingFunc(Real64 const &x, Real64 const &a, Real64 const &b);

    Real64 linInterp(Real64 const &x, Real64 const &x_l, Real64 const &x_h, Real64 const &y_l, Real64 const &y_h);

    void GetGroundHeatExchangerInput();

    std::shared_ptr<GLHEResponseFactors> BuildAndGetResponseFactorObjectFromArray(std::shared_ptr<GLHEVertArray> const &arrayObjectPtr);

    std::shared_ptr<GLHEResponseFactors> BuildAndGetResponseFactorsObjectFromSingleBHs(std::vector<std::shared_ptr<BHStruct>> const &singleBHsForRFVect);

    void SetupBHPointsForResponseFactorsObject(std::shared_ptr<GLHEResponseFactors> &thisRF);

    std::shared_ptr<GLHEResponseFactors> GetResponseFactor(std::string const &objectName);

    std::shared_ptr<BHStruct> GetSingleBH(std::string const &objectName);

    std::shared_ptr<BHPropsStruct> GetVertProps(std::string const &objectName);

    std::shared_ptr<GLHEVertArray> GetVertArray(std::string const &objectName);

    std::vector<Real64> solveTDM(std::vector<Real64> a, std::vector<Real64> b, std::vector<Real64> c, std::vector<Real64> d);

} // namespace GroundHeatExchangers

} // namespace EnergyPlus

#endif
