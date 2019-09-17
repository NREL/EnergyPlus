// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHeatBalance.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <IndoorIceRink.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {
namespace IceRink {
    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using DataGlobals::BeginTimeStepFlag;
    using DataGlobals::WarmupFlag;
    using DataHeatBalance::Construct;
    using DataHeatBalFanSys::QRadSysSource;
    using DataSurfaces::Surface;
    using DataSurfaces::TotSurfaces;
    using Psychrometrics::PsyTdpFnWPb;

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // System types:
    static std::string const BlankString;
    std::string const cDRink("IndoorIceRink:DirectRefrigerationSystem");
    std::string const cIRink("IndoorIceRink:IndirectRefrigerationSystem");
    int const DirectSystem(1);   // Direct refrigeration type radiant system
    int const IndirectSystem(2); // Indirect refrigeration type radiant system

    // Fluid types in indirect refrigeration system
    int const CaCl2(1); // Calcium chloride solution
    int const EG(2);    // Ethylene Glycol solution

    // Control types:
    int const SurfaceTempControl(1);     // Controls system using ice surface temperature
    int const BrineOutletTempControl(2); // Controls system using brine outlet temperature
                                         // Limit temperatures to indicate that a system cannot cool
    Real64 HighTempCooling(200.0);       // Used to indicate that a user does not have a cooling control temperature

    // Condensation control types:
    int const CondCtrlNone(0);      // Condensation control--none, so system never shuts down
    int const CondCtrlSimpleOff(1); // Condensation control--simple off, system shuts off when condensation predicted
    int const CondCtrlVariedOff(2); // Condensation control--variable off, system modulates to keep running if possible
    // Number of Circuits per Surface Calculation Method
    int const OneCircuit(1);          // there is 1 circuit per surface
    int const CalculateFromLength(2); // The number of circuits is TubeLength*SurfaceFlowFrac / CircuitLength
    std::string const OnePerSurf("OnePerSurface");
    std::string const CalcFromLength("CalculateFromCircuitLength");

    static std::string const fluidNameWater("WATER");
    static std::string const fluidNameBrine("BRINE");
    static std::string const fluidNameAmmonia("NH3");

    // MODULE VARIABLE DECLARATIONS:
    bool GetInputFlag = true;
    int TotalNumRefrigSystem(0); // Total number of refrigeration systems
    Array1D_bool CheckEquipName;
    int NumOfDirectRefrigSys(0);   // Number of direct refrigeration type ice rinks
    int NumOfIndirectRefrigSys(0); // Number of indirect refrigeration type ice rinks

    // Object Data
    Array1D<DirectRefrigSysData> DRink;
    Array1D<IndirectRefrigSysData> IRink;
    Array1D<RefrigSysTypeData> RefrigSysTypes;
    Array1D<ResurfacerData> Resurfacer;

    // Functions:

    void SimIndoorIceRink(std::string &Name,             // name of the refrigeration system
                          std::string &ResurfacerName,   // name of the resurfacer
                          bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                          Real64 &LoadMet,               // Load met by the refrigeration system, in Watts
                          int const RefrigType,          // Type of refrigerant used in the indirect type refrigeration system
                          int &CompIndex,
                          int &ResurfacerIndex)
    {
        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SysNum;        // Refrigeration system number/index in local derived types
        int SystemType;    // Type of refrigeration system: Direct or Indirect
        int ResurfacerNum; // Resurfacer machine number/index in local derived types
        bool InitErrorFound(false);

        // FLOW:
        if (GetInputFlag) {
            GetIndoorIceRink();
            GetInputFlag = false;
        }

        // Find what type of system is it, is it a direct refrigeration system
        // or indirect refrigeration system ?
        if (CompIndex == 0) {
            SysNum = UtilityRoutines::FindItemInList(Name, RefrigSysTypes);
            if (SysNum == 0) {
                ShowFatalError("SimIndoorIceRink: Unit not found =" + Name);
            }
            CompIndex = SysNum;
            SystemType = RefrigSysTypes(SysNum).SystemType;
            {
                auto const SELECT_CASE_var(SystemType);
                if (SELECT_CASE_var == DirectSystem) {
                    RefrigSysTypes(SysNum).CompIndex = UtilityRoutines::FindItemInList(Name, DRink);
                } else if (SELECT_CASE_var == IndirectSystem) {
                    RefrigSysTypes(SysNum).CompIndex = UtilityRoutines::FindItemInList(Name, IRink);
                }
            }

        } else {
            SysNum = CompIndex;
            SystemType = RefrigSysTypes(SysNum).SystemType;
            if (SysNum > TotalNumRefrigSystem || SysNum < 1) {
                ShowFatalError("SimIndoorIceRink:  Invalid CompIndex passed=" + TrimSigDigits(SysNum) +
                               ", Number of Units=" + TrimSigDigits(TotalNumRefrigSystem) + ", Entered Unit name=" + Name);
            }
            if (CheckEquipName(SysNum)) {
                if (Name != RefrigSysTypes(SysNum).Name) {
                    ShowFatalError("SimIndoorIceRink: Invalid CompIndex passed=" + TrimSigDigits(SysNum) + ", Unit name=" + Name +
                                   ", stored Unit Name for that index=" + RefrigSysTypes(SysNum).Name);
                }
                CheckEquipName(SysNum) = false;
            }
        }

        InitIndoorIceRink(FirstHVACIteration, InitErrorFound, SystemType, SysNum, ResurfacerIndex);
        if (InitErrorFound) {
            ShowFatalError("InitLowTempRadiantSystem: Preceding error is not allowed to proceed with the simulation.  Correct this input problem.");
        }

        {
            auto const SELECT_CASE_var(SystemType);
            if (SystemType == DirectSystem) {
                CalcDirectIndoorIceRinkSys(RefrigSysTypes(SysNum).CompIndex, LoadMet);
            } else if (SystemType == IndirectSystem) {
                CalcIndirectIndoorIceRinkSys(RefrigSysTypes(SysNum).CompIndex, LoadMet, RefrigType);
            } else {
                ShowFatalError("SimIndoorIceRink: Illegal system type for system " + Name);
            }
        }

        UpdateIndoorIceRink(SysNum, SystemType);

        ReportIndoorIceRink(SysNum, SystemType);
    }

    void GetIndoorIceRink()
    {
        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reads the input for indoor ice rinks from the user
        // input file.  This will contain all of the information
        // needed to simulate a indoor ice rink.

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using DataGlobals::ScheduleAlwaysOn;
        using DataHeatBalance::Zone;
        using DataSizing::AutoSize;
        using DataSurfaces::HeatTransferModel_CTF;
        using DataSurfaces::SurfaceClass_Floor;
        using DataSurfaces::SurfaceClass_Window;
        using FluidProperties::FindGlycol;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetScheduleIndex;
        using namespace DataLoopNode;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const MinThrottlingRange(0.5); // Smallest throttling range allowed in degrees Celsius
        static std::string const RefrigOutletTemperature("RefrigOutletTemperature");
        static std::string const IceSurfaceTemperature("IceSurfaceTemperature");
        static std::string const RoutineName("GetIndoorIceRink");
        static std::string const Off("Off");
        static std::string const SimpleOff("SimpleOff");
        static std::string const VariableOff("VariableOff");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool ErrorsFound(false);  // Set to true if something goes wrong
        std::string CurrentModuleObject; // for ease in getting objects
        Array1D_string Alphas;           // Alpha items for object
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        int GlycolIndex;                 // Index of 'Refrigerant' in glycol data structure
        int IOStatus;                    // Used in GetObjectItem
        int Item;                        // Item to be "gotten"
        int MaxAlphas;                   // Maximum number of alphas for these input keywords
        int MaxNumbers;                  // Maximum number of numbers for these input keywords
        Array1D<Real64> Numbers;         // Numeric items for object
        int NumAlphas;                   // Number of Alphas for each GetObjectItem call
        int NumArgs;                     // Unused variable that is part of a subroutine call
        int NumNumbers;                  // Number of Numbers for each GetObjectItem call
        int BaseNum;                     // Temporary number for creating RefrigSystemTypes structure
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
        int SurfNum;                     // Surface number

        // FLOW:
        // Initializations and allocations
        MaxAlphas = 0;
        MaxNumbers = 0;

        inputProcessor->getObjectDefMaxArgs("IndoorIceRink:DirectRefrigSystem", NumArgs, NumAlphas, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNumbers);

        inputProcessor->getObjectDefMaxArgs("IndoorIceRink:IndirectRefrigSystem", NumArgs, NumAlphas, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNumbers);

        Alphas.allocate(MaxAlphas);
        Numbers.dimension(MaxNumbers, 0.0);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNumbers);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNumbers, true);

        NumOfDirectRefrigSys = inputProcessor->getNumObjectsFound("IndoorIceRink:DirectRefrigSystem");
        NumOfIndirectRefrigSys = inputProcessor->getNumObjectsFound("IndoorIceRink:IndirectRefrigSystem");

        TotalNumRefrigSystem = NumOfDirectRefrigSys + NumOfIndirectRefrigSys;
        RefrigSysTypes.allocate(TotalNumRefrigSystem);
        CheckEquipName.dimension(TotalNumRefrigSystem, true);

        DRink.allocate(NumOfDirectRefrigSys);
        if (NumOfDirectRefrigSys > 0) {
            GlycolIndex = FindGlycol(fluidNameAmmonia);
            for (auto &e : DRink)
                e.GlycolIndex = GlycolIndex;
            if (GlycolIndex == 0) {
                ShowSevereError("Direct Refrigeration systems: no refrigerant(ammonia) property data found in input");
                ErrorsFound = true;
            }
        } else {
            for (auto &e : DRink)
                e.GlycolIndex = 0;
        }

        IRink.allocate(NumOfIndirectRefrigSys);
        if (NumOfIndirectRefrigSys > 0) {
            GlycolIndex = FindGlycol(fluidNameBrine);
            for (auto &e : IRink)
                e.GlycolIndex = GlycolIndex;
            if (GlycolIndex == 0) {
                ShowSevereError("Indirect Refrigeration systems: no refrigerant(ammonia) property data found in input");
                ErrorsFound = true;
            }
        } else {
            for (auto &e : IRink)
                e.GlycolIndex = 0;
        }

        // Obtain all the user data related to direct refrigeration type indoor ice rink
        BaseNum = 0;
        CurrentModuleObject = "IndoorIceRink:DirectRefrigSystem";
        for (Item = 1; Item <= NumOfDirectRefrigSys; ++Item) {

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Item,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);
            ++BaseNum;
            RefrigSysTypes(BaseNum).Name = Alphas(1);
            RefrigSysTypes(BaseNum).SystemType = DirectSystem;
            // General user input data
            DRink(Item).Name = Alphas(1);
            DRink(Item).SchedName = Alphas(2);
            if (lAlphaBlanks(2)) {
                DRink(Item).SchedPtr = ScheduleAlwaysOn;
            } else {
                DRink(Item).SchedPtr = GetScheduleIndex(Alphas(2));
                if (DRink(Item).SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " not found for " + Alphas(1));
                    ShowContinueError("Missing " + cAlphaFields(2) + " is " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            DRink(Item).ZoneName = Alphas(3);
            DRink(Item).ZonePtr = UtilityRoutines::FindItemInList(Alphas(3), Zone);
            if (DRink(Item).ZonePtr == 0) {
                ShowSevereError(RoutineName + "Invalid " + cAlphaFields(3) + " = " + Alphas(3));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            DRink(Item).SurfaceName = Alphas(4);
            DRink(Item).SurfacePtr = 0;
            for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (UtilityRoutines::SameString(Surface(SurfNum).Name, DRink(Item).SurfaceName)) {
                    DRink(Item).SurfacePtr = SurfNum;
                    break;
                }
            }
            if (DRink(Item).SurfacePtr <= 0) {
                ShowSevereError(RoutineName + "Invalid " + cAlphaFields(4) + " = " + Alphas(4));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            } else if (Surface(DRink(Item).SurfacePtr).HeatTransferAlgorithm != HeatTransferModel_CTF) {
                ShowSevereError(Surface(DRink(Item).SurfacePtr).Name +
                                " is an ice rink floor and is attempting to use a non-CTF solution algorithm.  This is "
                                "not allowed.  Use the CTF solution algorithm for this surface.");
                ErrorsFound = true;
            } else if (Surface(DRink(Item).SurfacePtr).Class == SurfaceClass_Window) {
                ShowSevereError(
                    Surface(DRink(Item).SurfacePtr).Name +
                    " is an ice rink floor and is defined as a window.  This is not allowed.  A pool must be a floor that is NOT a window.");
                ErrorsFound = true;
            } else if (Surface(DRink(Item).SurfacePtr).Class == !SurfaceClass_Floor) {
                ShowSevereError(Surface(DRink(Item).SurfacePtr).Name +
                                " is an ice rink floor and is defined as not a floor.  This is not allowed.  A rink must be a floor.");
                ErrorsFound = true;
            } else if (Surface(DRink(Item).SurfacePtr).Construction == 0) {
                ShowSevereError(Surface(DRink(Item).SurfacePtr).Name + " has an invalid construction");
                ErrorsFound = true;
            } else if (!Construct(Surface(DRink(Item).SurfacePtr).Construction).SourceSinkPresent) {
                ShowSevereError("Construction referenced in Direct Refrigeration System Surface does not have a source/sink present");
                ShowContinueError("Surface name= " + Surface(DRink(Item).SurfacePtr).Name +
                                  "  Construction name = " + Construct(Surface(DRink(Item).SurfacePtr).Construction).Name);
                ShowContinueError("Construction needs to be defined with a \"Construction:InternalSource\" object.");
                ErrorsFound = true;
            } else {
                DRink(Item).NumOfSurfaces = 1;
                DRink(Item).SurfacePtr = 1;
                DRink(Item).SurfaceName = (DRink(Item).NumOfSurfaces);
                DRink(Item).SurfaceFlowFrac.allocate(DRink(Item).NumOfSurfaces);
                DRink(Item).NumCircuits.allocate(DRink(Item).NumOfSurfaces);
                DRink(Item).SurfaceFlowFrac(1) = 1.0;
                DRink(Item).NumCircuits(1) = 0.0;
            }

            DRink(Item).TubeDiameter = Numbers(1);
            DRink(Item).TubeLength = Numbers(2);

            // Process the temperature control type
            if (UtilityRoutines::SameString(Alphas(5), RefrigOutletTemperature)) {
                DRink(Item).ControlType = BrineOutletTempControl;
            } else if (UtilityRoutines::SameString(Alphas(5), IceSurfaceTemperature)) {
                DRink(Item).ControlType = SurfaceTempControl;
            } else {
                ShowWarningError("Invalid " + cAlphaFields(5) + " =" + Alphas(5));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Control reset to MAT control for this Hydronic Radiant System.");
                DRink(Item).ControlType = SurfaceTempControl;
            }

            // Cooling user input data
            DRink(Item).RefrigVolFlowMaxCool = Numbers(3);

            DRink(Item).ColdRefrigInNode = GetOnlySingleNode(
                Alphas(6), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Unknown, NodeConnectionType_Inlet, 1, ObjectIsNotParent);

            DRink(Item).ColdRefrigOutNode = GetOnlySingleNode(
                Alphas(7), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Unknown, NodeConnectionType_Inlet, 1, ObjectIsNotParent);

            if ((!lAlphaBlanks(6)) || (!lAlphaBlanks(7))) {
                TestCompSet(CurrentModuleObject, Alphas(1), Alphas(6), Alphas(7), "Chilled Refrigerant Nodes");
            }

            DRink(Item).ColdThrottleRange = Numbers(4);
            if (DRink(Item).ColdThrottleRange < MinThrottlingRange) {
                ShowWarningError("IndoorIceRink:DirectRefrigSystem: Cooling throttling range too small, reset to 0.5");
                ShowContinueError("Occurs in Refrigeration System=" + DRink(Item).Name);
                DRink(Item).ColdThrottleRange = MinThrottlingRange;
            }

            DRink(Item).ColdSetptSched = Alphas(8);
            DRink(Item).ColdSetptSchedPtr = GetScheduleIndex(Alphas(8));
            if ((DRink(Item).ColdSetptSchedPtr == 0) && (!lAlphaBlanks(8))) {
                ShowSevereError(cAlphaFields(8) + " not found: " + Alphas(8));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
            if (UtilityRoutines::SameString(Alphas(9), Off)) {
                DRink(Item).CondCtrlType = CondCtrlNone;
            } else if (UtilityRoutines::SameString(Alphas(9), SimpleOff)) {
                DRink(Item).CondCtrlType = CondCtrlSimpleOff;
            } else if (UtilityRoutines::SameString(Alphas(9), VariableOff)) {
                DRink(Item).CondCtrlType = CondCtrlVariedOff;
            } else {
                DRink(Item).CondCtrlType = CondCtrlSimpleOff;
            }

            DRink(Item).CondDewPtDeltaT = Numbers(5);

            if (UtilityRoutines::SameString(Alphas(10), OnePerSurf)) {
                DRink(Item).NumCircCalcMethod = OneCircuit;
            } else if (UtilityRoutines::SameString(Alphas(10), CalcFromLength)) {
                DRink(Item).NumCircCalcMethod = CalculateFromLength;
            } else {
                DRink(Item).NumCircCalcMethod = OneCircuit;
            }

            DRink(Item).CircLength = Numbers(6);

            if ((DRink(Item).RefrigVolFlowMaxCool == AutoSize) &&
                (lAlphaBlanks(6) || lAlphaBlanks(7) || lAlphaBlanks(8) || (DRink(Item).ColdRefrigInNode <= 0) ||
                 (DRink(Item).ColdRefrigOutNode <= 0) || (DRink(Item).ColdSetptSchedPtr == 0))) {
                ShowSevereError("Direct Refrigeration systems may not be autosized without specification of nodes or schedules");
                ShowContinueError("Occurs in " + CurrentModuleObject + " (cooling input) =" + Alphas(1));
                ErrorsFound = true;
            }
        }

        CurrentModuleObject = "IndoorIceRink:IndirectRefrigSystem";
        for (Item = 1; Item <= NumOfIndirectRefrigSys; ++Item) {
        }

        Alphas.deallocate();
        Numbers.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();
    }

} // namespace IceRink
} // namespace EnergyPlus
