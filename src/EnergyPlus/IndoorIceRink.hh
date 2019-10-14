#ifndef IceRink_hh_INCLUDED
#define IceRink_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace IceRink {

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // System types:
    extern int const DirectSystem;
    extern int const IndirectSystem;
    extern std::string const cDRink;
    extern std::string const cIRink;

    // Fluid types in indirect refrigeration system
    extern int const CaCl2;
    extern int const EG;

    // Control types:
    extern int const SurfaceTempControl;
    extern int const BrineOutletTempControl;
    extern Real64 HighTempCooling;

    // Operating Mode:
    extern int NotOperating; // Parameter for use with OperatingMode variable, set for not operating
    extern int CoolingMode;  // Parameter for use with OperatingMode variable, set for cooling

    // Condensation control types:
    extern int const CondCtrlNone;
    extern int const CondCtrlSimpleOff;
    extern int const CondCtrlVariedOff;

    // Number of Circuits per Surface Calculation Method
    extern int const OneCircuit;
    extern int const CalculateFromLength;
    extern std::string const OnePerSurf;
    extern std::string const CalcFromLength;

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE DECLARATIONS:
    // Standard, run-of-the-mill variables...
    extern int NumOfDirectRefrigSys;
    extern int NumOfIndirectRefrigSys;
    extern int TotalNumRefrigSystem;
    extern int NumOfResurfacer;
    extern int OperatingMode;                    // Used to keep track of whether system is in heating or cooling mode
    extern Array1D<Real64> QRadSysSrcAvg;        // Average source over the time step for a particular radiant surface
    extern Array1D<Real64> ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
    extern Array1D_bool CheckEquipName;

    struct DirectRefrigSysData
    {
        // Members
        // Input data
        std::string Name; // name of direct system
        std::string SchedName; // availability schedule
        int SchedPtr;          // index to schedule
        std::string ZoneName;  // Name of zone the system is present
        int ZonePtr;      // Pointer to the zone in which the floor radiant system is present
        std::string SurfaceName; // surface name of rink
        int RefrigLoopNum;
        int RefrigLoopSide;
        int RefrigBranchNum;
        int RefrigCompNum;
        int RefrigIndex;                     // Index to refrigerant (NH3) properties
        int ControlStrategy;                 // Control strategy for the ice rink (BOTC or STC)
        Real64 MinRefrigMassFlow;            // Minimum mass flow rate of refrigerant allowed in the floor radiant system(kg/s)
        Real64 MaxRefrigMassFlow;            // Miximum mass flow rate of refrigerant allowed in the floor radiant system(kg/s)
        int NumCircuits;                     // Number of fluid circuits in the floor radiant system
        Real64 TubeLength;                   // Length of the piping used in the floor radiant system
        Real64 TubeDiameter;                 // Diameter of the piping used in the floor radiant system
        int RefrigInNode;                    // Inlet node number for the cold refrigerant entring the floor radiant system
        int RefrigOutNode;                   // Outlet node number for the hot refrigerant exiting the floor radiant system
        int RefrigSetptSchedPtr;             // Pointer to set point temperature of refrigerant outlet
        int IceSetptSchedPtr;                // Pointer to set point temperature of ice surface
        Real64 RefrigSetptTemp;              // Set point temperature for refrigerant outlet
        Real64 IceSetptTemp;                 // Set point temperature for ice surface
        std::string RefrigSetptSched;        // Set point schedule name for BOTC
        std::string IceSetptSched;           // Set point schedule name for STC
        int NumOfSurfaces;                   // Total number of surfaces in the ice rink arena
        Array1D_int SurfacePtrArray;         // index to a surface array
        Real64 TotalSurfaceArea;             // Surface area of the rink
        //Real64 RefrigVolFlowMaxCool;         // maximum refrigerant flow rate for cooling, m3/s
        //Real64 RefrigFlowMaxCool;            // maximum refrigerant flow rate for cooling, kg/s
        int PeopleHeatGainSchedPtr;          // People schedule index
        std::string PeopleHeatGainSchedName; // Name of people heat gain schedule
        std::string PeopleSchedName;         // Name of people schedule
        int PeopleSchedPtr;                  // People schedule index
        Real64 PeopleHeatGain;               // Current heat gain from people
        Real64 MaxNumOfPeople;               // Number of people in the rink as defined by user input
        Real64 LengthRink;
        Real64 WidthRink;
        Real64 DepthRink;
        Real64 IceThickness;
        int WaterIndex;
        Real64 FloodWaterTemp;

        // ReportData
        Real64 RefrigInletTemp;   // Refrigerant inlet temperature
        Real64 RefrigOutletTemp;  // Refrigerant outlet temperature
        Real64 RefrigMassFlow;    // Refrigerant mass flow rate in the floor radiant system(Kg/s)
        Real64 CoolPower;         // Cooling sent to rink floor in Watts
        Real64 CoolEnergy;        // Cooling sent to rink floor in Joules
        int OutRangeHiErrorCount; // recurring errors for crazy results too high fluid temperature
        int OutRangeLoErrorCount; // recurring errors for crazy results too low fluid temperature
        // Default Constructor
        DirectRefrigSysData()
            : SchedPtr(0), ZonePtr(0), RefrigLoopNum(0), RefrigLoopSide(0), RefrigBranchNum(0), RefrigCompNum(0), RefrigIndex(0),
              ControlStrategy(0), MinRefrigMassFlow(0.0), MaxRefrigMassFlow(0.0), NumCircuits(0), TubeLength(0.0), TubeDiameter(0.0), RefrigInNode(0),
              RefrigOutNode(0), RefrigSetptSchedPtr(0), RefrigSetptTemp(0.0), IceSetptTemp(0.0), IceSetptSchedPtr(0), NumOfSurfaces(0),
              TotalSurfaceArea(0.0), PeopleHeatGainSchedPtr(0), PeopleSchedPtr(0), PeopleHeatGain(0.0), MaxNumOfPeople(0), LengthRink(0.0), WidthRink(0.0), DepthRink(0.0),
              IceThickness(0.0), WaterIndex(0), FloodWaterTemp(0.0), RefrigInletTemp(0.0), RefrigOutletTemp(0.0), RefrigMassFlow(0.0), CoolPower(0.0),
              CoolEnergy(0.0), OutRangeHiErrorCount(0), OutRangeLoErrorCount(0)

        {
        }
    };

    struct IndirectRefrigSysData
    {
        // Members
        // Input Data
        std::string Name; // name of indirect system
        std::string SchedName;      // availability schedule
        int SchedPtr;          // index to schedule
        std::string ZoneName;       // Name of zone the system is present
        int ZonePtr;      // Pointer to the zone in which the floor radiant system is present
        int RefrigLoopNum;
        int RefrigLoopSide;
        int RefrigBranchNum;
        int RefrigCompNum;
        std::string SurfaceName;             // surface name of rink
        int GlycolIndex1;                    // Index to Ethylene Glycol properties
        int GlycolIndex2;                    // Index to Calcium Chloride properties
        int ControlStrategy;                 // Control strategy for the ice rink (BOTC or STC)
        Real64 MinRefrigMassFlow;            // Minimum mass flow rate of refrigerant allowed in the floor radiant system(kg/s)
        Real64 MaxRefrigMassFlow;            // Miximum mass flow rate of refrigerant allowed in the floor radiant system(kg/s)
        int NumCircuits;                     // Number of fluid circuits in the floor radiant system
        Real64 TubeLength;                   // Length of the piping used in the floor radiant system
        Real64 TubeDiameter;                 // Diameter of the piping used in the floor radiant system
        int RefrigInNode;                    // Inlet node number for the cold refrigerant entring the floor radiant system
        int RefrigOutNode;                   // Outlet node number for the hot refrigerant exiting the floor radiant system
        int RefrigSetptSchedPtr;             // Pointer to set point temperature of refrigerant outlet
        Real64 RefrigSetptTemp;              // Set point temperature for refrigerant outlet
        Real64 IceSetptTemp;                 // Set point temperature for ice surface
        int IceSetptSchedPtr;                // Pointer to set point temperature of ice surface
        std::string RefrigSetptSched;        // Set point schedule name for BOTC
        std::string IceSetptSched;           // Set point schedule name for STC
        int NumOfSurfaces;                   // Total number of surfaces in the ice rink arena
        Array1D_int SurfacePtrArray;         // index to a surface array
        Real64 TotalSurfaceArea;             // Surface area of the rink
        //Real64 RefrigVolFlowMaxCool;         // maximum refrigerant flow rate for cooling, m3/s
        //Real64 RefrigFlowMaxCool;            // maximum refrigerant flow rate for cooling, kg/s
        int PeopleHeatGainSchedPtr;          // People schedule index
        std::string PeopleHeatGainSchedName; // Name of people heat gain schedule
        std::string PeopleSchedName;         // Name of people schedule
        int PeopleSchedPtr;                  // People schedule index
        Real64 PeopleHeatGain;               // Current heat gain from people
        Real64 MaxNumOfPeople;               // Number of people in the rink as defined by user input
        Real64 LengthRink;
        Real64 WidthRink;
        Real64 DepthRink;
        Real64 IceThickness;
        int WaterIndex;
        Real64 FloodWaterTemp;
        int RefrigType;
        Real64 RefrigConc;
        

        // ReportData
        Real64 RefrigInletTemp;   // Refrigerant inlet temperature
        Real64 RefrigOutletTemp;  // Refrigerant outlet temperature
        Real64 RefrigMassFlow;    // Refrigerant mass flow rate in the floor radiant system(Kg/s)
        Real64 CoolPower;         // Cooling sent to rink floor in Watts
        Real64 CoolEnergy;        // Cooling sent to rink floor in Joules
        int OutRangeHiErrorCount; // recurring errors for crazy results too high fluid temperature
        int OutRangeLoErrorCount; // recurring errors for crazy results too low fluid temperature
        // Default Constructor
        IndirectRefrigSysData() : SchedPtr(0), ZonePtr(0), RefrigLoopNum(0), RefrigLoopSide(0), 
            RefrigBranchNum(0), RefrigCompNum(0), GlycolIndex1(0), GlycolIndex2(0),
            ControlStrategy(0), MinRefrigMassFlow(0.0), MaxRefrigMassFlow(0.0), NumCircuits(0), 
            TubeLength(0.0), TubeDiameter(0.0), RefrigInNode(0), RefrigOutNode(0), 
            RefrigSetptSchedPtr(0), RefrigSetptTemp(0.0), IceSetptTemp(0.0), IceSetptSchedPtr(0), 
            NumOfSurfaces(0), TotalSurfaceArea(0.0), PeopleHeatGainSchedPtr(0), PeopleSchedPtr(0), 
            PeopleHeatGain(0.0), MaxNumOfPeople(0), LengthRink(0.0), WidthRink(0.0), DepthRink(0.0), 
            IceThickness(0.0), WaterIndex(0), FloodWaterTemp(0.0), RefrigType(0), RefrigConc(0.0), 
            RefrigInletTemp(0.0), RefrigOutletTemp(0.0), RefrigMassFlow(0.0), CoolPower(0.0), 
            CoolEnergy(0.0), OutRangeHiErrorCount(0), OutRangeLoErrorCount(0)

        {
        }
    };

    struct RefrigSysTypeData
    {
        // Members
        // This type used to track different components/types for efficiency
        std::string Name; // name of refrigeartion system
        int SystemType;   // Type of System (see System Types in Parameters)
        int CompIndex;    // Index in specific system types

        // Default Constructor
        RefrigSysTypeData() : SystemType(0), CompIndex(0)  
        {
        }
    };

    struct ResurfacerData
    {
        // Members
        std::string Name;
        int CompIndex;
        int GlycolIndex;
        std::string ResurfacingSchedName;
        int ResurfacingSchedPtr;
        int NoOfResurfEvents;
        Real64 ResurfacingWaterTemp;
        Real64 InitWaterTemp;
        Real64 TankCapacity; // in ltr

        // Report Data
        Real64 ResurfacingHeatLoad;
        Real64 QResurfacing;
        Real64 EHeatingWater;
        Real64 QHumidity;
        // Default Constructor
        ResurfacerData() : CompIndex(0), GlycolIndex(0), ResurfacingSchedPtr(0), 
            NoOfResurfEvents(0), ResurfacingWaterTemp(0.0), InitWaterTemp(0.0),
            TankCapacity(0.0), QResurfacing(0.0), EHeatingWater(0.0), QHumidity(0.0), 
            ResurfacingHeatLoad(0.0)
        {
        }
    };

    // Object Data:
    extern Array1D<DirectRefrigSysData> DRink;
    extern Array1D<IndirectRefrigSysData> IRink;
    extern Array1D<RefrigSysTypeData> RefrigSysTypes;
    extern Array1D<ResurfacerData> Resurfacer;

    // Functions:
    void SimIndoorIceRink(std::string const &CompName,
                          std::string const &ResurfacerName,
                          bool const FirstHVACIteration,
                          Real64 &LoadMet,
                          int &CompIndex,
                          int &ResurfacerIndex);

    void GetIndoorIceRink();

    void InitIndoorIceRink(bool const FirstHVACIteration, int const SysNum, int const SystemType, bool &InitErrorsFound);

    void IceRinkFreezing(Real64 &FreezingLoad, int const SysNum, int const SystemType);

    void IceRinkResurfacer(Real64 &ResurfacerHeatLoad, int const SysNum, int const SystemType, int const MachineNum);

    Real64 CalcEffectivenessDRink(int const SysNum,
                                  Real64 const Temperature,
                                  Real64 const RefrigMassFlow,
                                  Real64 const NumCircs,
                                  Real64 const TubeLength,
                                  Real64 const TubeDiameter);

    Real64 CalcEffectivenessIRink(int const SysNum,
                                  Real64 const Temperature,
                                  Real64 const RefrigMassFlow,
                                  Real64 const NumCircs,
                                  Real64 const TubeLength,
                                  Real64 const TubeDiameter,
                                  Real64 const RefrigType,
                                  Real64 const Concentration);

    void CalcIndirectIndoorIceRink(int const SysNum, int const MachineNum, Real64 &LoadMet);

    void CalcDirectIndoorIceRink(int const SysNum, int const MachineNum, Real64 &LoadMet);

    void UpdateIndoorIceRink(bool const FirstHVACIteration, int const RadSysNum, int const SystemType);

    void UpdateRinkRadSysSourceValAvg(bool &FloorRadSysOn);

    void CheckForOutOfRangeTempResult(
        int const SystemType, int const SysNum, Real64 const outletTemp, Real64 const inletTemp, Real64 const EP_UNUSED(mdot));

    Real64 SumHATsurf(int const ZoneNum);

    void ReportIndoorIceRink(int const SysNum, int const SystemType);
} // namespace IceRink
} // namespace EnergyPlus

#endif
