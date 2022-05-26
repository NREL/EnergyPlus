// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

#ifndef AIRFLOWNETWORK_SOLVER_HPP
#define AIRFLOWNETWORK_SOLVER_HPP

// define this variable to get new code, commenting should yield original
#define SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS

// C++ Headers
#include <unordered_map>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <AirflowNetwork/Properties.hpp>
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace AirflowNetwork {

    // Vent Control  DistSys Control  Flag    Description
    //  NONE           NONE           0      No AirflowNetwork and SIMPLE
    //  SIMPLE         NONE           1      Simple calculations only
    //  MULTIZONE      NONE           2      Perform multizone calculations only
    //  NONE           DISTSYS        3      Perform distribution system during system on time only
    //  SIMPLE         DISTSYS        4      Perform distribution system during system on time and simple calculations during off time
    //  MULTIZONE      DISTSYS        5      Perform distribution system during system on time and multizone calculations during off time
    //
    // int constexpr AirflowNetworkControlSimple(1);    // Simple calculations only
    // int constexpr AirflowNetworkControlMultizone(2); // Perform multizone calculations only
    // int constexpr AirflowNetworkControlSimpleADS(4); // Perform distribution system during system on time and simple calculations during off time
    // int constexpr AirflowNetworkControlMultiADS(5);  // Perform distribution system during system on time and multizone calculations during off
    // time

    enum class ControlType
    {
        NoMultizoneOrDistribution = 0,                   // Simple calculations only, AirflowNetworkControlSimple(1)
        MultizoneWithoutDistribution,                    // Perform multizone calculations only, AirflowNetworkControlMultizone(2)
        MultizoneWithDistributionOnlyDuringFanOperation, // Perform distribution system during system on time and simple calculations during off time,
                                                         // AirflowNetworkControlSimpleADS(4)
        MultizoneWithDistribution // Perform distribution system during system on time and multizone calculations during off time,
                                  // AirflowNetworkControlMultiADS(5)
    };

    struct AirflowNetworkReportVars
    {
        // Members
        Real64 InfilVolume;         // Volume of Air {m3} due to infiltration
        Real64 InfilMass;           // Mass of Air {kg} due to infiltration
        Real64 InfilAirChangeRate;  // Infiltration air change rate {ach}
        Real64 VentilHeatLoss;      // Heat Gain {W} due to ventilation
        Real64 VentilHeatGain;      // Heat Loss {W} due to ventilation
        Real64 VentilVolume;        // Volume of Air {m3} due to ventilation
        Real64 VentilMass;          // Mass of Air {kg} due to ventilation
        Real64 VentilAirChangeRate; // Ventilation air change rate {ach}
        Real64 VentilFanElec;       // Fan Electricity {W} due to ventilation
        Real64 VentilAirTemp;       // Air Temp {C} of ventilation
        Real64 MixVolume;           // Mixing volume of Air {m3}
        Real64 MixMass;             // Mixing mass of air {kg}
        Real64 ExfilSensiLoss;      // Sensible heat Loss rate {W} due to exfiltration
        Real64 ExfilLatentLoss;     // Latent heat Loss rate {W} due to exfiltration
        Real64 ExfilTotalLoss;      // Total heat Loss rate {W} due to exfiltration
        Real64 ExfilMass;           // Mass of Air {kg} due to exfiltration
        Real64 InletMass;           // Total zone inlet mass of air {kg}
        Real64 OutletMass;          // Total zone outlet mass of air {kg}

        // Default Constructor
        AirflowNetworkReportVars()
            : InfilVolume(0.0), InfilMass(0.0), InfilAirChangeRate(0.0), VentilHeatLoss(0.0), VentilHeatGain(0.0), VentilVolume(0.0), VentilMass(0.0),
              VentilAirChangeRate(0.0), VentilFanElec(0.0), VentilAirTemp(0.0), MixVolume(0.0), MixMass(0.0), ExfilSensiLoss(0.0),
              ExfilLatentLoss(0.0), ExfilTotalLoss(0.0), ExfilMass(0.0), InletMass(0.0), OutletMass(0.0)
        {
        }
    };

    Real64 AFNPressureResidual(EnergyPlusData &state, Real64 ExFanMassFlowRate,
                               Array1D<Real64> const &Par); // Residual function using Regula Falsi

    // derived class or struct
    struct OccupantVentilationControlProp
    {

        std::string Name;                     // Provide a unique object name
        Real64 MinOpeningTime;                // Minimum Opening Time
        Real64 MinClosingTime;                // Minimum Closing Time
        std::string ComfortLowTempCurveName;  // Thermal Comfort Low Temperature Curve Name
        std::string ComfortHighTempCurveName; // Thermal Comfort High Temperature Curve Name
        int ComfortLowTempCurveNum;           // Thermal Comfort Low Temperature Curve number
        int ComfortHighTempCurveNum;          // Thermal Comfort high Temperature Curve number
        int OpeningProbSchNum;                // Opening probability schedule pointer
        int ClosingProbSchNum;                // Closing probability schedule pointer
        Real64 ComfortBouPoint;               // Thermal Comfort Temperature Boundary Point
        bool OccupancyCheck;                  // Occupancy check
        std::string OpeningProbSchName;       // Opening probability schedule name
        std::string ClosingProbSchName;       // Closing probability schedule name
        Real64 MaxPPD;                        // Maximum PPD used to calculate comfort band (%)
        bool MinTimeControlOnly;              // Chach minimum opening and closing time only

        // Default Constructor
        OccupantVentilationControlProp()
            : MinOpeningTime(0.0), MinClosingTime(0.0), ComfortLowTempCurveNum(0), ComfortHighTempCurveNum(0), OpeningProbSchNum(0),
              ClosingProbSchNum(0), ComfortBouPoint(10.0), OccupancyCheck(false), MaxPPD(10.0), MinTimeControlOnly(false)
        {
        }

        void calc(EnergyPlusData &state,
                  int ZoneNum,
                  Real64 TimeOpenDuration,
                  Real64 TimeCloseDuration,
                  int &OpeningStatus,
                  int &OpeningProbStatus,
                  int &ClosingProbStatus); // function to perform calculations

        bool opening_probability(EnergyPlusData &state,
                                 int ZoneNum,
                                 Real64 TimeCloseDuration); // function to perform calculations of opening probability

        bool closing_probability(EnergyPlusData &state, Real64 TimeCloseDuration); // function to perform calculations of closing probability
    };

    struct SimulationControl // Basic parameters for AirflowNetwork simulation
    {
        enum class Solver
        {
            SkylineLU,
            ConjugateGradient
        };

        // Members
        std::string name; // Provide a unique object name
        ControlType type =
            ControlType::NoMultizoneOrDistribution; // AirflowNetwork control: MULTIZONE WITH DISTRIBUTION, MULTIZONE WITHOUT DISTRIBUTION, MULTIZONE
                                                    // WITH DISTRIBUTION ONLY DURING FAN OPERATION, and NO MULTIZONE OR DISTRIBUTION
        std::string WPCCntr = "SURFACEAVERAGECALCULATION"; // Wind pressure coefficient input control: "SURFACE-AVERAGE CALCULATION", or "INPUT"
        iWPCCntr iWPCCnt = iWPCCntr::SurfAvg;              // Integer equivalent for WPCCntr field
        std::string BldgType = "LowRise";                  // Building type: "LOWRISE" or "HIGHRISE" at WPCCntr = "SURFACE-AVERAGE CALCULATIO"
        std::string HeightOption = "OpeningHeight";        // Height Selection: "ExternalNode" or "OpeningHeight" at WPCCntr = "INPUT"
        int maximum_iterations = 500;                      // Maximum number of iteration, default 500
        int InitFlag = 1;                                  // Initialization flag
        Solver solver = Solver::SkylineLU;
        Real64 relative_convergence_tolerance = 0.0001; // Relative airflow convergence
        Real64 absolute_convergence_tolerance = 1e-06;  // Absolute airflow convergence
        Real64 convergence_acceleration_limit = -0.5;   // Convergence acceleration limit
        Real64 MaxPressure = 500.0;                     // Maximum pressure change in an element [Pa]
        Real64 azimuth = 0.0;                           // Azimuth Angle of Long Axis of Building, not used at WPCCntr = "INPUT"
        Real64 aspect_ratio = 1.0;                      // Ratio of Building Width Along Short Axis to Width Along Long Axis
        // Real64 DiffP;                          // Minimum pressure difference
        std::string InitType = "ZeroNodePressures";    // Initialization flag type:
        bool temperature_height_dependence = false;    // Choice of height dependence of external node temperature
        bool allow_unsupported_zone_equipment = false; // Allow unsupported zone equipment
        // "ZeroNodePressures", or "LinearInitializationMethod"
    };

    struct Solver : BaseGlobalStruct
    {

        Solver(EnergyPlusData &state);

        void initialize();
        void calculate_Cps();
        void allocate();
        void initialize_calculation();
        void setsky();
        void airmov();
        void solvzp(int &ITER);      // number of iterations
        void filjac(int const NNZE,  // number of nonzero entries in the "AU" array.
                    bool const LFLAG // if = 1, use laminar relationship (initialization).
        );
        void facsky(Array1D<Real64> &AU,   // the upper triangle of [A] before and after factoring
                    Array1D<Real64> &AD,   // the main diagonal of [A] before and after factoring
                    Array1D<Real64> &AL,   // the lower triangle of [A] before and after factoring
                    const Array1D_int &IK, // pointer to the top of column/row "K"
                    int const NEQ,         // number of equations
                    int const NSYM         // symmetry:  0 = symmetric matrix, 1 = non-symmetric
        );

        void slvsky(const Array1D<Real64> &AU, // the upper triangle of [A] before and after factoring
                    const Array1D<Real64> &AD, // the main diagonal of [A] before and after factoring
                    const Array1D<Real64> &AL, // the lower triangle of [A] before and after factoring
                    Array1D<Real64> &B,        // "B" vector (input); "X" vector (output).
                    const Array1D_int &IK,     // pointer to the top of column/row "K"
                    int const NEQ,             // number of equations
                    int const NSYM             // symmetry:  0 = symmetric matrix, 1 = non-symmetric
        );

        void filsky(const Array1D<Real64> &X,    // element array (row-wise sequence)
                    std::array<int, 2> const LM, // location matrix
                    const Array1D_int &IK,       // pointer to the top of column/row "K"
                    Array1D<Real64> &AU,         // the upper triangle of [A] before and after factoring
                    Array1D<Real64> &AD,         // the main diagonal of [A] before and after factoring
                    int const FLAG               // mode of operation
        );

        void manage_balance(Optional_bool_const FirstHVACIteration = _, // True when solution technique on first iteration
                            Optional_int_const Iter = _,                // Iteration number
                            Optional_bool ResimulateAirZone = _         // True when solution technique on third iteration
        );

        void get_input();

        void allocate_and_initialize();

        void calculate_balance();

        Real64 calculate_wind_pressure(int curve,           // Curve index, change this to pointer after curve refactor
                                       bool symmetricCurve, // True if the curve is symmetric (0 to 180)
                                       bool relativeAngle,  // True if the Cp curve angle is measured relative to the surface
                                       Real64 azimuth,      // Azimuthal angle of surface
                                       Real64 windSpeed,    // Wind velocity
                                       Real64 windDir,      // Wind direction
                                       Real64 dryBulbTemp,  // Air node dry bulb temperature
                                       Real64 humRat        // Air node humidity ratio
        );

        Real64 duct_inside_convection_resistance(Real64 Tair, // Average air temperature
                                                 Real64 mdot, // Mass flow rate
                                                 Real64 Dh,   // Hydraulic diameter
                                                 Real64 hIn   // User defined convection coefficient
        );

        Real64 duct_outside_convection_resistance(Real64 Ts,   // Surface temperature
                                                  Real64 Tamb, // Free air temperature
                                                  Real64 Wamb, // Free air humidity ratio
                                                  Real64 Pamb, // Free air barometric pressure
                                                  Real64 Dh,   // Hydraulic diameter
                                                  int ZoneNum, // Zone number
                                                  Real64 hOut  // User defined convection coefficient
        );

        void calculate_heat_balance();
        void calculate_moisture_balance();
        void calculate_CO2_balance();
        void calculate_GC_balance();
        void mrxinv(int NORDER);
        void report();
        void update(Optional_bool_const FirstHVACIteration = _); // True when solution technique on first iteration
        void venting_control(int i,                              // AirflowNetwork surface number
                             Real64 &OpenFactor                  // Window or door opening factor (used to calculate airflow)
        );
        void assign_fan_airloop();
        void validate_distribution();
        void validate_fan_flowrate(); // Catch a fan flow rate from EPlus input file and add a flag for VAV terminal damper
        void validate_exhaust_fan_input();
        void hybrid_ventilation_control();
        void single_sided_Cps(std::vector<std::vector<Real64>> &valsByFacade, int numWindDirs = 36);
        Real64 zone_OA_change_rate(int ZoneNum); // hybrid ventilation system controlled zone number
        int get_airloop_number(int NodeNumber);  // Get air loop number for each distribution node and linkage

        EPVector<OccupantVentilationControlProp> OccupantVentilationControl;
        Array1D_int SplitterNodeNumbers;
        int AirflowNetworkNumOfExtSurfaces = 0;
        // Inverse matrix
        Array1D<Real64> MA;
        Array1D<Real64> MV;
        Array1D_int IVEC;
        int VentilationCtrl = 0;  // Hybrid ventilation control type
        int NumOfExhaustFans = 0; // Number of exhaust fans
        int NumAirflowNetwork = 0;
        int AirflowNetworkNumOfDetOpenings = 0;
        int AirflowNetworkNumOfSimOpenings = 0;
        int AirflowNetworkNumOfHorOpenings = 0;
        int AirflowNetworkNumOfSurCracks = 0;
        int AirflowNetworkNumOfSurELA = 0;
        int AirflowNetworkNumOfSFR = 0;
        int AirflowNetworkNumOfExtNode = 0;
        int AirflowNetworkNumOfOutAirNode = 0;
        int AirflowNetworkNumOfSingleSideZones = 0; // Total number of zones with advanced single sided wind pressure coefficient calculation
        int DisSysNumOfNodes = 0;
        int DisSysNumOfLeaks = 0;
        int DisSysNumOfELRs = 0;
        int DisSysNumOfDucts = 0;
        int DisSysNumOfDuctViewFactors = 0;
        int DisSysNumOfDampers = 0;
        int DisSysNumOfCVFs = 0;
        int DisSysNumOfDetFans = 0;
        int DisSysNumOfCoils = 0;
        int DisSysNumOfHXs = 0;
        int DisSysNumOfCPDs = 0;
        int DisSysNumOfTermUnits = 0;
        int DisSysNumOfLinks = 0;
        int NumOfExtNodes = 0;
        Real64 IncAng = 0.0;                     // Wind incidence angle relative to facade normal (deg)
        int SupplyFanType = 0;                   // Supply air fan type
        Real64 MaxOnOffFanRunTimeFraction = 0.0; // max Run time fraction for an On/Off fan flow rate among airloops
        Real64 CurrentEndTimeLast = 0.0;         // last end time
        Real64 TimeStepSysLast = 0.0;            // last system time step
        int AirflowNetworkNumOfOccuVentCtrls = 0;
        int IntraZoneNumOfNodes = 0;
        int IntraZoneNumOfLinks = 0;
        int IntraZoneNumOfZones = 0;
        int NumOfPressureControllers = 0; // number of pressure controllers
        int NumOfOAFans = 0;              // number of OutdoorAir fans
        int NumOfReliefFans = 0;          // number of OutdoorAir relief fans
        bool AirflowNetworkGetInputFlag = true;
        bool AssignFanAirLoopNumFlag = true;
        bool ValidateDistributionSystemFlag = true;
        std::array<Real64, 5> FacadeAng; // Facade azimuth angle (for walls, angle of outward normal to facade measured clockwise from North) (deg)
        Array1D<Real64> LoopPartLoadRatio;
        Array1D<Real64> LoopOnOffFanRunTimeFraction;
        Array1D<bool> LoopOnOffFlag;

        bool ValidateExhaustFanInputOneTimeFlag = true;
        bool initializeOneTimeFlag = true;
        bool initializeMyEnvrnFlag = true;
        bool CalcAirflowNetworkAirBalanceOneTimeFlag = true;
        bool CalcAirflowNetworkAirBalanceErrorsFound = false;
        bool UpdateAirflowNetworkMyOneTimeFlag = true;
        bool UpdateAirflowNetworkMyOneTimeFlag1 = true;

        // CalcAirflowNetworkAirBalance variables
        int ErrCountVar = 0;
        int ErrCountHighPre = 0;
        int ErrCountLowPre = 0;
        int ErrIndexHighPre = 0;
        int ErrIndexVar = 0;
        int ErrIndexLowPre = 0;

        // Object Data
        EPVector<AirflowNetwork::AirflowNetworkReportVars> AirflowNetworkZnRpt;
        std::unordered_map<std::string, std::string> UniqueAirflowNetworkSurfaceName;

        // Output and reporting
        EPVector<AirflowNetwork::AirflowNetworkExchangeProp> exchangeData;
        EPVector<AirflowNetwork::AirflowNetworkExchangeProp> multiExchangeData;
        EPVector<AirflowNetwork::AirflowNetworkLinkReportData> linkReport;
        EPVector<AirflowNetwork::AirflowNetworkNodeReportData> nodeReport;
        EPVector<AirflowNetwork::AirflowNetworkLinkReportData> linkReport1;

        // used to be statics
        Array1D<bool> onceZoneFlag;
        Array1D<bool> onceSurfFlag;
        bool onetime = false;
        int HybridGlobalErrIndex = 0;
        int HybridGlobalErrCount = 0;
        int AFNNumOfExtOpenings = 0; // Total number of external openings in the model
        int OpenNuminZone = 0;       // Counts which opening this is in the zone, 1 or 2

        std::unordered_map<std::string, AirflowElement *> elements;
        std::unordered_map<std::string, int> compnum; // Stopgap until all the introspection is dealt with

        AirProperties properties;

        // int const NrInt; // Number of intervals for a large opening

        AirflowNetwork::DetailedOpeningSolver dos;

        // Data
        int ActualNumOfLinks = 0;
        int ActualNumOfNodes = 0;

        // Common block AFEDAT
        Array1D<Real64> AFECTL;
        Array1D<Real64> AFLOW2;
        Array1D<Real64> AFLOW;
        Array1D<Real64> PS;
        Array1D<Real64> PW;

        // Common block CONTRL
        Real64 PB = 0.0;

        // Common block ZONL
        // Array1D<Real64> RHOZ;
        // Array1D<Real64> SQRTDZ;
        // Array1D<Real64> VISCZ;
        Array1D<Real64> SUMAF;
        // Array1D<Real64> TZ; // Temperature [C]
        // Array1D<Real64> WZ; // Humidity ratio [kg/kg]
        Array1D<Real64> PZ; // Pressure [Pa]

        // Other array variables
        Array1D_int ID;
        Array1D_int IK;
        Array1D<Real64> AD;
        Array1D<Real64> AU;

#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
        Array1D_int newIK;     // noel
        Array1D<Real64> newAU; // noel
#endif

        // REAL(r64), ALLOCATABLE, DIMENSION(:) :: AL
        Array1D<Real64> SUMF;

        Array1D_bool AirflowNetworkZoneFlag;
        int NumOfNodesMultiZone = 0;    // Number of nodes for multizone calculation
        int NumOfNodesDistribution = 0; // Number of nodes for distribution system calculation
        int NumOfLinksMultiZone = 0;    // Number of links for multizone calculation
        int NumOfLinksDistribution = 0; // Number of links for distribution system calculation
        int NumOfNodesIntraZone = 0;    // Number of nodes for intrazone calculation
        int NumOfLinksIntraZone = 0;    // Number of links for intrazone calculation

        int AirflowNetworkNumOfNodes = 0; // Number of nodes for AirflowNetwork calculation
        // = NumOfNodesMultiZone+NumOfNodesDistribution
        int AirflowNetworkNumOfComps = 0; // Number of components for AirflowNetwork calculation
        int AirflowNetworkNumOfLinks = 0; // Number of links for AirflowNetwork calculation
        // = NumOfLinksMultiZone+NumOfLinksDistribution
        // RoomAirManager use
        int AirflowNetworkNumOfSurfaces = 0; // The number of surfaces for multizone calculation
        int AirflowNetworkNumOfZones = 0;    // The number of zones for multizone calculation

        bool RollBackFlag = false;                 // Roll back flag when system time step down shifting
        Array1D<Real64> ANZT;                      // Local zone air temperature for roll back use
        Array1D<Real64> ANZW;                      // Local zone air humidity ratio for roll back use
        Array1D<Real64> ANCO;                      // Local zone air CO2 for roll back use
        Array1D<Real64> ANGC;                      // Local zone air generic contaminant for roll back use
        int AirflowNetworkNumOfExhFan = 0;         // Number of zone exhaust fans
        Array1D_bool AirflowNetworkZoneExhaustFan; // Logical to use zone exhaust fans
        bool AirflowNetworkFanActivated = false;   // Supply fan activation flag
        bool AirflowNetworkUnitarySystem = false;  // set to TRUE for unitary systems (to make answers equal, will remove eventually)
        // Multispeed HP only
        int MultiSpeedHPIndicator = 0; // Indicator for multispeed heat pump use
        // Additional airflow needed for an VAV fan to compensate the leakage losses and supply pathway pressure losses [kg/s]
        Real64 VAVTerminalRatio = 0.0;       // The terminal flow ratio when a supply VAV fan reach its max flow rate
        bool VAVSystem = false;              // This flag is used to represent a VAV system
        Real64 ExhaustFanMassFlowRate = 0.0; // Exhaust fan flow rate used in PressureStat
        int PressureSetFlag = 0;             // PressureSet flag
        Real64 ReliefMassFlowRate = 0.0;     // OA Mixer relief node flow rate used in PressureStat
        bool control_defaulted = false;      // Default simulation control flag

        Array1D<AirflowNetwork::AirflowNetworkNodeSimuData> AirflowNetworkNodeSimu;
        Array1D<AirflowNetwork::AirflowNetworkLinkSimuData> AirflowNetworkLinkSimu;

        SimulationControl simulation_control;
        bool distribution_simulated = false;     // True if distribution is being simulated
        bool multizone_always_simulated = false; // True if the multizone solution is always computed

        // Moved from simulation control to outer object
        int ExtLargeOpeningErrCount = 0; // Exterior large opening error count during HVAC system operation
        int ExtLargeOpeningErrIndex = 0; // Exterior large opening error index during HVAC system operation
        int OpenFactorErrCount = 0;      // Large opening error count at Open factor > 1.0
        int OpenFactorErrIndex = 0;      // Large opening error error index at Open factor > 1.0

        EPVector<Node> nodes;
        EPVector<AirflowNetwork::AirflowNetworkCompProp> AirflowNetworkCompData;
        EPVector<AirflowNetwork::AirflowNetworkLinkageProp> AirflowNetworkLinkageData;
        EPVector<AirflowNetwork::MultizoneZoneProp> MultizoneZoneData;
        EPVector<AirflowNetwork::MultizoneSurfaceProp> MultizoneSurfaceData;
        EPVector<AirflowNetwork::DetailedOpening> MultizoneCompDetOpeningData;
        EPVector<AirflowNetwork::SimpleOpening> MultizoneCompSimpleOpeningData;
        EPVector<AirflowNetwork::HorizontalOpening> MultizoneCompHorOpeningData;
        EPVector<AirflowNetwork::SurfaceCrack> MultizoneSurfaceCrackData;
        EPVector<AirflowNetwork::EffectiveLeakageArea> MultizoneSurfaceELAData;
        EPVector<AirflowNetwork::SpecifiedMassFlow> SpecifiedMassFlowData;
        EPVector<AirflowNetwork::SpecifiedVolumeFlow> SpecifiedVolumeFlowData;
        EPVector<AirflowNetwork::MultizoneExternalNodeProp> MultizoneExternalNodeData;
        EPVector<AirflowNetwork::DeltaCpProp> DeltaCp;
        EPVector<AirflowNetwork::DeltaCpProp> EPDeltaCP;
        EPVector<AirflowNetwork::ZoneExhaustFan> MultizoneCompExhaustFanData;
        EPVector<AirflowNetwork::IntraZoneNodeProp> IntraZoneNodeData;       // Intra zone data set
        EPVector<AirflowNetwork::IntraZoneLinkageProp> IntraZoneLinkageData; // Intra zone linkage adat set
        EPVector<AirflowNetwork::DisSysNodeProp> DisSysNodeData;
        EPVector<AirflowNetwork::DuctLeak> DisSysCompLeakData;
        EPVector<AirflowNetwork::EffectiveLeakageRatio> DisSysCompELRData;
        EPVector<AirflowNetwork::Duct> DisSysCompDuctData;
        EPVector<AirflowNetwork::Damper> DisSysCompDamperData;
        EPVector<AirflowNetwork::ConstantVolumeFan> DisSysCompCVFData;
        EPVector<AirflowNetwork::DetailedFan> DisSysCompDetFanData;
        EPVector<AirflowNetwork::DisSysCompCoilProp> DisSysCompCoilData;
        EPVector<AirflowNetwork::DisSysCompHXProp> DisSysCompHXData;
        EPVector<AirflowNetwork::DisSysCompTermUnitProp> DisSysCompTermUnitData;
        EPVector<AirflowNetwork::ConstantPressureDrop> DisSysCompCPDData;
        EPVector<AirflowNetwork::AiflowNetworkReportProp> AirflowNetworkReportData;
        EPVector<AirflowNetwork::PressureControllerProp> PressureControllerData;
        EPVector<AirflowNetwork::OutdoorAirFan> DisSysCompOutdoorAirData;
        EPVector<AirflowNetwork::ReliefFlow> DisSysCompReliefAirData;
        EPVector<AirflowNetwork::AirflowNetworkLinkageViewFactorProp> AirflowNetworkLinkageViewFactorData;

        void clear_state() override
        {
            OccupantVentilationControl.deallocate();
            SplitterNodeNumbers.deallocate();
            AirflowNetworkNumOfExtSurfaces = 0;
            MA.deallocate();
            MV.deallocate();
            IVEC.deallocate();
            VentilationCtrl = 0;
            NumOfExhaustFans = 0;
            NumAirflowNetwork = 0;
            AirflowNetworkNumOfDetOpenings = 0;
            AirflowNetworkNumOfSimOpenings = 0;
            AirflowNetworkNumOfHorOpenings = 0;
            AirflowNetworkNumOfSurCracks = 0;
            AirflowNetworkNumOfSurELA = 0;
            AirflowNetworkNumOfSFR = 0;
            AirflowNetworkNumOfExtNode = 0;
            AirflowNetworkNumOfOutAirNode = 0;
            AirflowNetworkNumOfSingleSideZones = 0;
            DisSysNumOfNodes = 0;
            DisSysNumOfLeaks = 0;
            DisSysNumOfELRs = 0;
            DisSysNumOfDucts = 0;
            DisSysNumOfDuctViewFactors = 0;
            DisSysNumOfDampers = 0;
            DisSysNumOfCVFs = 0;
            DisSysNumOfDetFans = 0;
            DisSysNumOfCoils = 0;
            DisSysNumOfHXs = 0;
            DisSysNumOfCPDs = 0;
            DisSysNumOfTermUnits = 0;
            DisSysNumOfLinks = 0;
            NumOfExtNodes = 0;
            IncAng = 0.0;
            SupplyFanType = 0;
            MaxOnOffFanRunTimeFraction = 0.0;
            CurrentEndTimeLast = 0.0;
            TimeStepSysLast = 0.0;
            AirflowNetworkNumOfOccuVentCtrls = 0;
            IntraZoneNumOfNodes = 0;
            IntraZoneNumOfLinks = 0;
            IntraZoneNumOfZones = 0;
            NumOfPressureControllers = 0;
            NumOfOAFans = 0;
            NumOfReliefFans = 0;
            AirflowNetworkGetInputFlag = true;
            AssignFanAirLoopNumFlag = true;
            ValidateDistributionSystemFlag = true;
            FacadeAng = {0.0, 0.0, 0.0, 0.0, 0.0};
            AirflowNetworkZnRpt.deallocate();
            LoopPartLoadRatio.deallocate();
            LoopOnOffFanRunTimeFraction.deallocate();
            LoopOnOffFlag.deallocate();
            UniqueAirflowNetworkSurfaceName.clear();
            ValidateExhaustFanInputOneTimeFlag = true;
            initializeOneTimeFlag = true;
            initializeMyEnvrnFlag = true;
            CalcAirflowNetworkAirBalanceOneTimeFlag = true;
            CalcAirflowNetworkAirBalanceErrorsFound = false;
            UpdateAirflowNetworkMyOneTimeFlag = true;
            UpdateAirflowNetworkMyOneTimeFlag1 = true;
            exchangeData.deallocate();
            multiExchangeData.deallocate();
            linkReport.deallocate();
            nodeReport.deallocate();
            linkReport1.deallocate();
            ErrCountVar = 0;
            ErrCountHighPre = 0;
            ErrCountLowPre = 0;
            ErrIndexHighPre = 0;
            ErrIndexVar = 0;
            ErrIndexLowPre = 0;
            onceZoneFlag.clear();
            onceSurfFlag.clear();
            onetime = false;
            HybridGlobalErrIndex = 0;
            HybridGlobalErrCount = 0;
            AFNNumOfExtOpenings = 0;
            OpenNuminZone = 0;

            ActualNumOfLinks = 0;
            ActualNumOfNodes = 0;
            AFECTL.clear();
            AFLOW2.clear();
            AFLOW.clear();
            PS.clear();
            PW.clear();
            PB = 0.0;
            SUMAF.clear();
            PZ.clear();
            ID.clear();
            IK.clear();
            AD.clear();
            AU.clear();
            elements.clear();
            compnum.clear();

#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
            newIK.deallocate();
            newAU.deallocate();
#endif
            SUMF.deallocate();
            dos.clear();
            properties.clear();

            AirflowNetworkNodeSimu.clear();
            AirflowNetworkLinkSimu.clear();
            AirflowNetworkZoneFlag.clear();
            NumOfNodesMultiZone = 0;
            NumOfNodesDistribution = 0;
            NumOfLinksMultiZone = 0;
            NumOfLinksDistribution = 0;
            NumOfNodesIntraZone = 0;
            NumOfLinksIntraZone = 0;
            AirflowNetworkNumOfNodes = 0;
            AirflowNetworkNumOfComps = 0;
            AirflowNetworkNumOfLinks = 0;
            AirflowNetworkNumOfSurfaces = 0;
            AirflowNetworkNumOfZones = 0;
            RollBackFlag = false;
            ANZT.clear();
            ANZW.clear();
            ANCO.clear();
            ANGC.clear();
            AirflowNetworkNumOfExhFan = 0;
            AirflowNetworkZoneExhaustFan.clear();
            AirflowNetworkFanActivated = false;
            AirflowNetworkUnitarySystem = false;
            MultiSpeedHPIndicator = 0;
            VAVTerminalRatio = 0.0;
            VAVSystem = false;
            simulation_control = AirflowNetwork::SimulationControl();
            distribution_simulated = false;
            multizone_always_simulated = false;
            ExtLargeOpeningErrCount = 0;
            ExtLargeOpeningErrIndex = 0;
            OpenFactorErrCount = 0;
            OpenFactorErrIndex = 0;
            nodes.clear();
            AirflowNetworkCompData.clear();
            AirflowNetworkLinkageData.clear();
            MultizoneZoneData.clear();
            MultizoneSurfaceData.clear();
            MultizoneCompDetOpeningData.clear();
            MultizoneCompSimpleOpeningData.clear();
            MultizoneCompHorOpeningData.clear();
            MultizoneSurfaceCrackData.clear();
            SpecifiedMassFlowData.clear();
            SpecifiedVolumeFlowData.clear();
            MultizoneSurfaceELAData.clear();
            MultizoneExternalNodeData.clear();
            DeltaCp.clear();
            EPDeltaCP.clear();
            MultizoneCompExhaustFanData.clear();
            IntraZoneNodeData.clear();    // Intra zone data set
            IntraZoneLinkageData.clear(); // Intra zone linkage adat set
            DisSysNodeData.clear();
            DisSysCompLeakData.clear();
            DisSysCompELRData.clear();
            DisSysCompDuctData.clear();
            DisSysCompDamperData.clear();
            DisSysCompCVFData.clear();
            DisSysCompDetFanData.clear();
            DisSysCompCoilData.clear();
            DisSysCompHXData.clear();
            DisSysCompTermUnitData.clear();
            DisSysCompCPDData.clear();
            AirflowNetworkReportData.clear();
            PressureControllerData.clear();
            DisSysCompOutdoorAirData.clear();
            DisSysCompReliefAirData.clear();
            AirflowNetworkLinkageViewFactorData.clear();
        }

    private:
        bool get_element_input();

        EnergyPlusData &m_state;
    };

} // namespace AirflowNetwork

} // namespace EnergyPlus

#endif
