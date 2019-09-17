#ifndef IceRink_hh_INCLUDED
#define IceRink_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace IceRink {

    struct DirectRefrigSysData
    {
        // Members
        // Input data
        std::string Name;                // name of direct refrigeration system
        std::string SchedName;           // availability schedule
        int SchedPtr;                    // index to schedule
        std::string ZoneName;            // Name of zone the system is serving
        int ZonePtr;                     // Point to this zone in the Zone derived type
        std::string SurfaceName;         // surface name of rink floor
        int SurfacePtr;                  // index to surface array
        int NumOfSurfaces;               // Number of surfaces included in this refrigeration system (coordinated control)
        Array1D<Real64> SurfaceFlowFrac; // Fraction of flow/pipe length for the floor surface
        Array1D<Real64> NumCircuits;     // Number of fluid circuits in the surface
        Real64 TubeDiameter;             // tube diameter for embedded tubing
        Real64 TubeLength;               // tube length embedded in radiant surface
        int ControlType;                 // Control type for the system(BOTC or STC)
        Real64 RefrigVolFlowMaxCool;     // maximum refrigerant flow rate for cooling, m3/s
        int ColdRefrigInNode;            // cold refrigerant inlet node
        int ColdRefrigOutNode;           // cold refrigerant Outlet node
        Real64 ColdThrottleRange;        // Throttling range for cooling [C]
        std::string ColdSetptSched;      // Schedule name for the ice rink setpoint temperature
        int ColdSetptSchedPtr;           // Schedule index for the ice rink setpoint temperature
        Real64 CondDewPtDeltaT;          // Diff between surface temperature and dew point for cond. shut-off
        int CondCtrlType;                // Condensation control type (initialize to simple off)
        int NumCircCalcMethod;           // Calculation method for number of circuits per surface; 1=1 per surface, 2=use cicuit length
        Real64 CircLength;               // Circuit length {m}
        int GlycolIndex;                 // Index to Glycol (Ammonia) Properties

        // ReportData

        // Default Constructor
        DirectRefrigSysData()
            : SchedPtr(0), ZonePtr(0), SurfacePtr(0), NumOfSurfaces(0), TubeDiameter(0.0), TubeLength(0.0), ControlType(0), RefrigVolFlowMaxCool(0.0),
              ColdRefrigInNode(0), ColdRefrigOutNode(0), ColdThrottleRange(0.0), ColdSetptSchedPtr(0), CondCtrlType(0), CondDewPtDeltaT(0.0),
              NumCircCalcMethod(0), CircLength(0.0), GlycolIndex(0)

        {
        }
    };

    struct IndirectRefrigSysData
    {
        // Members
        // Input Data
        std::string Name; // name of indirect refrigeration system
        int SchedPtr;     // index to schedule
        int GlycolIndex;  // Index to Glycol (Brine) Properties

        // Report Data

        // Default Constructor
        IndirectRefrigSysData() : SchedPtr(0), GlycolIndex(0)
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
        int GlycolIndex;
        int ResurfacingSchedPtr;
        Real64 ResurfacingWaterTemp;
        // Report Data
        Real64 QResurfacing;
        Real64 EHeatingWater;
        Real64 QHumidity;
        // Default Constructor
        ResurfacerData() : GlycolIndex(0), ResurfacingSchedPtr(0), ResurfacingWaterTemp(0.0), QResurfacing(0.0), EHeatingWater(0.0), QHumidity(0.0)
        {
        }
    };

    void GetIndoorIceRink();

    void InitIndoorIceRink(bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                           bool &InitErrorsFound,         // TRUE if some error is found in the initialization
                           int const SystemType,          // Type of refrigeration system: Direct or Indirect
                           int const SysNum,              // Index to the refrigeration system
                           int const ResurfacerIndex      // Index to the resurfacer
    );

    void CalcDirectIndoorIceRinkSys(int const SysNum, // name of the direct refrigeration system
                                    Real64 &LoadMet   // load met by the direct refrigeration system, in Watts
    );

    void CalcIndirectIndoorIceRinkSys(int const SysNum, // Index number for the direct refrigeration system
                                      Real64 &LoadMet,
                                      int const RefrigType // Type of refrigerant used in the indirect type refrigeration system
    );

    void UpdateIndoorIceRink(int const SysNum,    // index to the refrigeration system
                             int const SystemType // Type of refrigeration system: Direct or indirect
    );
    void ReportIndoorIceRink(int const SysNum,    // Index to the refrigeration system
                             int const SystemType // Type of refrigeration system: Direct or indirect
    );

} // namespace IceRink
} // namespace EnergyPlus

#endif
