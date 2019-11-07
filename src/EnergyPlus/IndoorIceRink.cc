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
#include <DataConversions.hh>
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

    // Operating Mode:
    int NotOperating(0); // Parameter for use with OperatingMode variable, set for not operating
    int CoolingMode(2);  // Parameter for use with OperatingMode variable, set for cooling

    static std::string const fluidNameWater("WATER");
    static std::string const fluidNameBrine("BRINE");
    static std::string const fluidNameAmmonia("NH3");

    // MODULE VARIABLE DECLARATIONS:
    bool GetInputFlag = true;
    int TotalNumRefrigSystem(0); // Total number of refrigeration systems
    Array1D_bool CheckEquipName;
    int NumOfDirectRefrigSys(0);          // Number of direct refrigeration type ice rinks
    int NumOfIndirectRefrigSys(0);        // Number of indirect refrigeration type ice rinks
    int NumOfResurfacer(0);               // Number of resurfacers
    bool FirstTimeInit(true);             // Set to true for first pass through init routine then set to false
    int OperatingMode(0);                 // Used to keep track of whether system is in heating or cooling mode
    Array1D<Real64> ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
    Array1D<Real64> QRadSysSrcAvg;        // Average source over the time step for a particular radiant surface
    // Record keeping variables used to calculate QRadSysSrcAvg locally
    Array1D<Real64> LastQRadSysSrc;     // Need to keep the last value in case we are still iterating
    Array1D<Real64> LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
    Array1D<Real64> LastTimeStepSys;    // Need to keep the last value in case we are still iterating

    // Object Data
    Array1D<DirectRefrigSysData> DRink;
    Array1D<IndirectRefrigSysData> IRink;
    Array1D<RefrigSysTypeData> RefrigSysTypes;
    Array1D<ResurfacerData> Resurfacer;

    // Functions:

    void SimIndoorIceRink(std::string const &CompName,       // name of the floor refrigeration system
                          std::string const &ResurfacerName, // Name of the resurfacer Machine
                          bool const FirstHVACIteration,     // TRUE if 1st HVAC simulation of system timestep
                          Real64 &LoadMet,                   // load met by the radiant system, in Watts
                          int &CompIndex,
                          int &ResurfacerIndex)
    {
        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SysNum;        // Refrigerant system number/index in local derived types
        int ResurfacerNum; // Resurfacer number/index in local derived types
        int SystemType;    // Type of refrigeration system: Direct or Indirect
        bool InitErrorFound(false);

        if (GetInputFlag) {
            GetIndoorIceRink();
            GetInputFlag = false;
        }

        if (CompIndex == 0) {
            SysNum = UtilityRoutines::FindItemInList(CompName, RefrigSysTypes);
            if (SysNum == 0) {
                ShowFatalError("SimIndoorIceRink: Unit not found =" + CompName);
            }
            CompIndex = SysNum;
            SystemType = RefrigSysTypes(SysNum).SystemType;
            {
                auto const SELECT_CASE_var(SystemType);
                if (SELECT_CASE_var == DirectSystem) {
                    RefrigSysTypes(SysNum).CompIndex = UtilityRoutines::FindItemInList(CompName, DRink);
                } else if (SELECT_CASE_var == IndirectSystem) {
                    RefrigSysTypes(SysNum).CompIndex = UtilityRoutines::FindItemInList(CompName, IRink);
                }
            }
        } else {
            SysNum = CompIndex;
            SystemType = RefrigSysTypes(SysNum).SystemType;
            if (SysNum > TotalNumRefrigSystem || SysNum < 1) {
                ShowFatalError("SimIndoorIceRink: Invalid CompIndex passed=" + TrimSigDigits(SysNum) +
                               ", Number of units=" + TrimSigDigits(TotalNumRefrigSystem) + ", Entered Unit name=" + CompName);
            }
            if (CheckEquipName(SysNum)) {
                if (CompName != RefrigSysTypes(SysNum).Name) {
                    ShowFatalError("SimIndoorIceRink: Invalid CompIndex passed=" + TrimSigDigits(SysNum) + ", Unit name=" + CompName +
                                   ", stored Unit Name for that index=" + RefrigSysTypes(SysNum).Name);
                }
                CheckEquipName(SysNum) = false;
            }
        }

        if (ResurfacerIndex == 0) {
            ResurfacerNum = UtilityRoutines::FindItemInList(ResurfacerName, Resurfacer);
            if (ResurfacerNum == 0) {
                ShowWarningMessage("SimIndoorIceRink: No resurfacer found =" + ResurfacerName);
            }
            ResurfacerIndex = ResurfacerNum;
            Resurfacer(ResurfacerNum).CompIndex = UtilityRoutines::FindItemInList(ResurfacerName, Resurfacer);
        } else {
            ResurfacerNum = ResurfacerIndex;
            if (ResurfacerNum <= 0) {
                ShowWarningMessage("SimIndoorIceRink: No resurfacer found");
            }
        }

        InitIndoorIceRink(FirstHVACIteration, RefrigSysTypes(SysNum).CompIndex, SystemType, InitErrorFound);
        if (InitErrorFound) {
            ShowFatalError("InitIndoorIceRink: Preceding error is not allowed to proceed with the simulation.  Correct this input problem.");
        }

        {
            auto const SELECT_CASE_var(SystemType);
            if (SELECT_CASE_var == DirectSystem) {
                CalcDirectIndoorIceRink(RefrigSysTypes(SysNum).CompIndex, Resurfacer(ResurfacerNum).CompIndex, LoadMet);
            } else if (SELECT_CASE_var == IndirectSystem) {
                CalcIndirectIndoorIceRink(RefrigSysTypes(SysNum).CompIndex, Resurfacer(ResurfacerNum).CompIndex, LoadMet);
            } else {
                ShowFatalError("SimIndoorIceRink: Illegal system type for system " + CompName);
            }
        }

        UpdateIndoorIceRink(FirstHVACIteration, RefrigSysTypes(SysNum).CompIndex, SystemType);

        ReportIndoorIceRink(RefrigSysTypes(SysNum).CompIndex, SystemType);
    }

    void GetIndoorIceRink()
    {
        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using DataGlobals::ScheduleAlwaysOn;
        using DataHeatBalance::Zone;
        using DataSurfaces::SurfaceClass_Floor;
        using DataSurfaces::SurfaceClass_Window;
        using FluidProperties::FindGlycol;
        using FluidProperties::FindRefrigerant;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetScheduleIndex;
        using namespace DataLoopNode;
        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetIndoorIceRink: ");
        static std::string const BOTC("BrineOutletTemperature");
        static std::string const STC("SurfaceTemperatureControl");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string CurrentModuleObject; // for ease in getting objects
        Array1D_string Alphas;           // Alpha items for object
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names

        static bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int RefrigIndex;                // Index of 'NH3' in refrigerant data structure
        int GlycolIndex1;               // Index of 'Ethylene Glycol' in glycol data structure
        int GlycolIndex2;               // Index of 'Calcium Chloride' in glycol data structure
        int WaterIndex;                 // Index to 'Water' in the glycol data structure
        int ResurfWaterIndex;           // Index to 'Water' in the glycol data structure for resurfacer
        int IOStatus;                   // Used in GetObjectItem
        int Item;                       // Item to be "gotten"
        int MaxAlphas;                  // Maximum number of alphas for these input keywords
        int MaxNumbers;                 // Maximum number of numbers for these input keywords
        Array1D<Real64> Numbers;        // Numeric items for object
        int NumAlphas;                  // Number of Alphas for each GetObjectItem call
        int NumArgs;                    // Unused variable that is part of a subroutine call
        int NumNumbers;                 // Number of Numbers for each GetObjectItem call

        int SurfNum;                 // DO loop counter for surfaces
        int SurfFloor;               // Surface which is a floor
        int BaseNum;                 // Temporary number for creating RadiantSystemTypes structure
        Array1D_bool lAlphaBlanks;   // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

        MaxAlphas = 0;
        MaxNumbers = 0;

        inputProcessor->getObjectDefMaxArgs("IceRink:Indoor:DirectSystem", NumArgs, NumAlphas, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNumbers);

        inputProcessor->getObjectDefMaxArgs("IceRink:Indoor:IndirectSystem", NumArgs, NumAlphas, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNumbers);

        inputProcessor->getObjectDefMaxArgs("IceRink:Resurfacer", NumArgs, NumAlphas, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNumbers);

        Alphas.allocate(MaxAlphas);
        Numbers.dimension(MaxNumbers, 0.0);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNumbers);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNumbers, true);

        NumOfDirectRefrigSys = inputProcessor->getNumObjectsFound("IceRink:Indoor:DirectSystem");
        NumOfIndirectRefrigSys = inputProcessor->getNumObjectsFound("IceRink:Indoor:IndirectSystem");
        NumOfResurfacer = inputProcessor->getNumObjectsFound("IceRink:Resurfacer");
        TotalNumRefrigSystem = NumOfDirectRefrigSys + NumOfIndirectRefrigSys;
        RefrigSysTypes.allocate(TotalNumRefrigSystem);

        DRink.allocate(NumOfDirectRefrigSys);
        if (NumOfDirectRefrigSys > 0) {
            WaterIndex = FindGlycol(fluidNameWater);
            RefrigIndex = FindRefrigerant("NH3");
            for (auto &e : DRink) {
                e.RefrigIndex = RefrigIndex;
                e.WaterIndex = WaterIndex;
            }
            if (RefrigIndex == 0) {
                ShowSevereError("Direct system: no NH3 data found in input");
                ErrorsFound = true;
            }
        } else {
            for (auto &e : DRink) {
                e.RefrigIndex = 0;
                e.WaterIndex = 0;
            }
        }

        IRink.allocate(NumOfIndirectRefrigSys);
        if (NumOfIndirectRefrigSys > 0) {
            WaterIndex = FindGlycol(fluidNameWater);
            GlycolIndex1 = FindGlycol("EthyleneGlycol");
            GlycolIndex2 = FindGlycol("CalciumChloride");
            for (auto &e : IRink) {
                e.GlycolIndex1 = GlycolIndex1;
                e.GlycolIndex2 = GlycolIndex2;
                e.WaterIndex = WaterIndex;
            }
            if ((GlycolIndex1 == 0) && (GlycolIndex2 == 0)) {
                ShowSevereError("Indirect system: no Glycol data found in input");
                ErrorsFound = true;
            }
        } else {
            for (auto &e : IRink) {
                e.GlycolIndex1 = 0;
                e.GlycolIndex2 = 0;
                e.WaterIndex = 0;
            }
        }

        Resurfacer.allocate(NumOfResurfacer);
        if (NumOfResurfacer > 0) {
            ResurfWaterIndex = FindGlycol(fluidNameWater);
            for (auto &e : Resurfacer)
                e.GlycolIndex = ResurfWaterIndex;
            if (ResurfWaterIndex == 0) {
                ShowSevereError("Resurfacer: no water data found in input");
                ErrorsFound = true;
            }
        } else {
            for (auto &e : Resurfacer)
                e.GlycolIndex = 0;
        }

        // Obtain all the user data related to direct type indoor ice rink...
        BaseNum = 0;
        CurrentModuleObject = "IceRink:Indoor:DirectSystem";
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
            DRink(Item).SurfacePtrArray.allocate(1);
            DRink(Item).SurfacePtrArray(1) = UtilityRoutines::FindItemInList(DRink(Item).SurfaceName, Surface);
            DRink(Item).NumCircuits = 0;
            for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (Surface(DRink(Item).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                    SurfFloor = DRink(Item).SurfacePtrArray(SurfNum);
                }
            }
            // Error checking for rink surface
            if (DRink(Item).SurfacePtrArray(SurfFloor) == 0) {
                ShowSevereError(RoutineName + "Invalid " + cAlphaFields(4) + " = " + Alphas(4));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            } else if (Surface(DRink(Item).SurfacePtrArray(SurfFloor)).PartOfVentSlabOrRadiantSurface) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", Invalid Surface");
                ShowContinueError(cAlphaFields(4) + "=\"" + Alphas(4) + "\" has been used in another radiant system or ventilated slab.");
                ErrorsFound = true;
            } else if (Surface(DRink(Item).SurfacePtrArray(SurfFloor)).Class == SurfaceClass_Window) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", Invalid Surface");
                ShowContinueError(cAlphaFields(4) + "=\"" + Alphas(4) + "\" is a window, which is not allowed.");
                ErrorsFound = true;
            } else if (!Construct(Surface(DRink(Item).SurfacePtrArray(SurfFloor)).Construction).SourceSinkPresent) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", Invalid Surface");
                ShowContinueError(cAlphaFields(4) + "=\"" + Alphas(4) + "\" has no source/sink present, which is not allowed.");
                ErrorsFound = true;
            }
            if (DRink(Item).SurfacePtrArray(SurfFloor) != 0) {
                Surface(DRink(Item).SurfacePtrArray(1)).IntConvSurfHasActiveInIt = true;
                Surface(DRink(Item).SurfacePtrArray(1)).PartOfVentSlabOrRadiantSurface = true;
            }

            if (Surface(DRink(Item).SurfacePtrArray(SurfFloor)).Zone != DRink(Item).ZonePtr) {
                ShowSevereError("Surface referenced in " + CurrentModuleObject +
                                " not in same zone as Refrigeration System, surface=" + Surface(DRink(Item).SurfacePtrArray(SurfFloor)).Name);
                ShowContinueError("Surface in Zone=" + Zone(Surface(DRink(Item).SurfacePtrArray(SurfFloor)).Zone).Name +
                                  " Direct refrigeration System in " + cAlphaFields(3) + " = " + Alphas(3));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            DRink(Item).TubeDiameter = Numbers(1);
            DRink(Item).TubeLength = Numbers(2);

            // Process the control strategy
            if (UtilityRoutines::SameString(Alphas(5), BOTC)) {
                DRink(Item).ControlStrategy = BrineOutletTempControl;
            } else if (UtilityRoutines::SameString(Alphas(5), STC)) {
                DRink(Item).ControlStrategy = SurfaceTempControl;
            } else {
                ShowWarningError("Invalid " + cAlphaFields(5) + " =" + Alphas(5));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Control reset to STC control for this Direct refrigeration System.");
                DRink(Item).ControlStrategy = SurfaceTempControl;
            }

            // Cooling user input data
            DRink(Item).MaxRefrigMassFlow = Numbers(3);
            DRink(Item).MinRefrigMassFlow = Numbers(4);
            DRink(Item).RefrigInNode = GetOnlySingleNode(
                Alphas(6), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Unknown, NodeConnectionType_Inlet, 1, ObjectIsNotParent);

            DRink(Item).RefrigOutNode = GetOnlySingleNode(
                Alphas(7), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Unknown, NodeConnectionType_Outlet, 1, ObjectIsNotParent);

            if ((!lAlphaBlanks(6)) || (lAlphaBlanks(7))) {
                TestCompSet(CurrentModuleObject, Alphas(1), Alphas(6), Alphas(7), "Refrigerant Nodes");
            }

            DRink(Item).RefrigSetptSched = Alphas(8);
            DRink(Item).RefrigSetptSchedPtr = GetScheduleIndex(Alphas(8));
            if ((DRink(Item).RefrigSetptSchedPtr == 0) && (!lAlphaBlanks(8))) {
                ShowSevereError(cAlphaFields(8) + " not found: " + Alphas(8));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            DRink(Item).IceSetptSched = Alphas(9);
            DRink(Item).IceSetptSchedPtr = GetScheduleIndex(Alphas(9));
            if ((DRink(Item).IceSetptSchedPtr == 0) && (!lAlphaBlanks(9))) {
                ShowSevereError(cAlphaFields(9) + " not found: " + Alphas(9));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            DRink(Item).PeopleHeatGainSchedName = Alphas(10);
            DRink(Item).PeopleHeatGainSchedPtr = GetScheduleIndex(Alphas(10));
            if ((DRink(Item).PeopleHeatGainSchedPtr == 0) && (!lAlphaBlanks(10))) {
                ShowSevereError(cAlphaFields(10) + " not found: " + Alphas(10));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            DRink(Item).PeopleSchedName = Alphas(11);
            DRink(Item).PeopleSchedPtr = GetScheduleIndex(Alphas(11));
            if ((DRink(Item).PeopleSchedPtr == 0) && (!lAlphaBlanks(11))) {
                ShowSevereError(cAlphaFields(11) + " not found: " + Alphas(11));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            DRink(Item).MaxNumOfPeople = Numbers(5);
            if (DRink(Item).MaxNumOfPeople < 0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + " was entered with negative people.  This is not allowed.");
                ShowContinueError("The number of people has been reset to zero.");
                DRink(Item).MaxNumOfPeople = 0.0;
            }

            DRink(Item).LengthRink = Numbers(6);
            if (DRink(Item).LengthRink <= 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive rink length. This is not allowed");
                ShowContinueError("The rink length has been reset to 60.");
                DRink(Item).LengthRink = 60.0;
            }

            DRink(Item).WidthRink = Numbers(7);
            if (DRink(Item).WidthRink <= 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive rink width. This is not allowed");
                ShowContinueError("The rink length has been reset to 30.");
                DRink(Item).WidthRink = 30.0;
            }

            DRink(Item).DepthRink = Numbers(8);
            if (DRink(Item).DepthRink <= 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive rink depth. This is not allowed");
                ShowContinueError("The rink length has been reset to 1.");
                DRink(Item).DepthRink = 1.0;
            }

            DRink(Item).IceThickness = Numbers(9);
            if (DRink(Item).IceThickness <= 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive ice thickness. This is not allowed");
                ShowContinueError("The rink length has been reset to 0.1.");
                DRink(Item).IceThickness = 0.0254;
            }

            DRink(Item).FloodWaterTemp = Numbers(10);
            if (DRink(Item).FloodWaterTemp <= 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive flood water temperature. This is not allowed");
                ShowContinueError("The rink length has been reset to 15.0.");
                DRink(Item).FloodWaterTemp = 15.0;
            }
        }

        //  Obtain all the user data related to indirect type indoor ice rink...

        CurrentModuleObject = "IceRink:Indoor:IndirectSystem";
        for (Item = 1; Item <= NumOfIndirectRefrigSys; ++Item) {

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
            RefrigSysTypes(BaseNum).SystemType = IndirectSystem;
            // General user input data
            IRink(Item).Name = Alphas(1);
            IRink(Item).SchedName = Alphas(2);
            if (lAlphaBlanks(2)) {
                IRink(Item).SchedPtr = ScheduleAlwaysOn;
            } else {
                IRink(Item).SchedPtr = GetScheduleIndex(Alphas(2));
                if (IRink(Item).SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " not found for " + Alphas(1));
                    ShowContinueError("Missing " + cAlphaFields(2) + " is " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            IRink(Item).ZoneName = Alphas(3);
            IRink(Item).ZonePtr = UtilityRoutines::FindItemInList(Alphas(3), Zone);
            if (IRink(Item).ZonePtr == 0) {
                ShowSevereError(RoutineName + "Invalid " + cAlphaFields(3) + " = " + Alphas(3));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            IRink(Item).SurfaceName = Alphas(4);
            IRink(Item).SurfacePtrArray.allocate(1);
            IRink(Item).SurfacePtrArray(1) = UtilityRoutines::FindItemInList(IRink(Item).SurfaceName, Surface);
            IRink(Item).NumCircuits = 0;
            for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (Surface(IRink(Item).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                    SurfFloor = IRink(Item).SurfacePtrArray(SurfNum);
                }
            }
            // Error checking for rink surface
            if (IRink(Item).SurfacePtrArray(SurfFloor) == 0) {
                ShowSevereError(RoutineName + "Invalid " + cAlphaFields(4) + " = " + Alphas(4));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            } else if (Surface(IRink(Item).SurfacePtrArray(SurfFloor)).PartOfVentSlabOrRadiantSurface) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", Invalid Surface");
                ShowContinueError(cAlphaFields(4) + "=\"" + Alphas(4) + "\" has been used in another radiant system or ventilated slab.");
                ErrorsFound = true;
            } else if (Surface(IRink(Item).SurfacePtrArray(SurfFloor)).Class == SurfaceClass_Window) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", Invalid Surface");
                ShowContinueError(cAlphaFields(4) + "=\"" + Alphas(4) + "\" is a window, which is not allowed.");
                ErrorsFound = true;
            } else if (!Construct(Surface(IRink(Item).SurfacePtrArray(SurfFloor)).Construction).SourceSinkPresent) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", Invalid Surface");
                ShowContinueError(cAlphaFields(4) + "=\"" + Alphas(4) + "\" has no source/sink present, which is not allowed.");
                ErrorsFound = true;
            }
            if (IRink(Item).SurfacePtrArray(SurfFloor) != 0) {
                Surface(IRink(Item).SurfacePtrArray(1)).IntConvSurfHasActiveInIt = true;
                Surface(IRink(Item).SurfacePtrArray(1)).PartOfVentSlabOrRadiantSurface = true;
            }

            if (Surface(IRink(Item).SurfacePtrArray(SurfFloor)).Zone != IRink(Item).ZonePtr) {
                ShowSevereError("Surface referenced in " + CurrentModuleObject +
                                " not in same zone as Refrigeration System, surface=" + Surface(IRink(Item).SurfacePtrArray(SurfFloor)).Name);
                ShowContinueError("Surface in Zone=" + Zone(Surface(IRink(Item).SurfacePtrArray(SurfFloor)).Zone).Name +
                                  " Direct refrigeration System in " + cAlphaFields(3) + " = " + Alphas(3));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            IRink(Item).TubeDiameter = Numbers(1);
            IRink(Item).TubeLength = Numbers(2);

            // Process the control strategy
            if (UtilityRoutines::SameString(Alphas(5), BOTC)) {
                IRink(Item).ControlStrategy = BrineOutletTempControl;
            } else if (UtilityRoutines::SameString(Alphas(5), STC)) {
                IRink(Item).ControlStrategy = SurfaceTempControl;
            } else {
                ShowWarningError("Invalid " + cAlphaFields(5) + " =" + Alphas(5));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Control reset to STC control for this Direct refrigeration System.");
                IRink(Item).ControlStrategy = SurfaceTempControl;
            }

            // Cooling user input data
            IRink(Item).MaxRefrigMassFlow = Numbers(3);
            IRink(Item).MinRefrigMassFlow = Numbers(4);
            IRink(Item).RefrigInNode = GetOnlySingleNode(
                Alphas(6), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Unknown, NodeConnectionType_Inlet, 1, ObjectIsNotParent);

            IRink(Item).RefrigOutNode = GetOnlySingleNode(
                Alphas(7), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Unknown, NodeConnectionType_Outlet, 1, ObjectIsNotParent);

            if ((!lAlphaBlanks(6)) || (lAlphaBlanks(7))) {
                TestCompSet(CurrentModuleObject, Alphas(1), Alphas(6), Alphas(7), "Refrigerant Nodes");
            }

            IRink(Item).RefrigSetptSched = Alphas(8);
            IRink(Item).RefrigSetptSchedPtr = GetScheduleIndex(Alphas(8));
            if ((IRink(Item).RefrigSetptSchedPtr == 0) && (!lAlphaBlanks(8))) {
                ShowSevereError(cAlphaFields(8) + " not found: " + Alphas(8));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            IRink(Item).IceSetptSched = Alphas(9);
            IRink(Item).IceSetptSchedPtr = GetScheduleIndex(Alphas(9));
            if ((IRink(Item).IceSetptSchedPtr == 0) && (!lAlphaBlanks(9))) {
                ShowSevereError(cAlphaFields(9) + " not found: " + Alphas(9));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            IRink(Item).PeopleHeatGainSchedName = Alphas(10);
            IRink(Item).PeopleHeatGainSchedPtr = GetScheduleIndex(Alphas(10));
            if ((IRink(Item).PeopleHeatGainSchedPtr == 0) && (!lAlphaBlanks(10))) {
                ShowSevereError(cAlphaFields(10) + " not found: " + Alphas(10));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            IRink(Item).PeopleSchedName = Alphas(11);
            IRink(Item).PeopleSchedPtr = GetScheduleIndex(Alphas(11));
            if ((IRink(Item).PeopleSchedPtr == 0) && (!lAlphaBlanks(11))) {
                ShowSevereError(cAlphaFields(11) + " not found: " + Alphas(11));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            IRink(Item).MaxNumOfPeople = Numbers(5);
            if (IRink(Item).MaxNumOfPeople < 0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + " was entered with negative people.  This is not allowed.");
                ShowContinueError("The number of people has been reset to zero.");
                IRink(Item).MaxNumOfPeople = 0.0;
            }

            IRink(Item).LengthRink = Numbers(6);
            if (IRink(Item).LengthRink <= 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive rink length. This is not allowed");
                ShowContinueError("The rink length has been reset to 60.");
                IRink(Item).LengthRink = 60.0;
            }

            IRink(Item).WidthRink = Numbers(7);
            if (IRink(Item).WidthRink <= 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive rink width. This is not allowed");
                ShowContinueError("The rink length has been reset to 30.");
                IRink(Item).WidthRink = 30.0;
            }

            IRink(Item).DepthRink = Numbers(8);
            if (IRink(Item).DepthRink <= 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive rink depth. This is not allowed");
                ShowContinueError("The rink length has been reset to 1.");
                IRink(Item).DepthRink = 1.0;
            }

            IRink(Item).IceThickness = Numbers(9);
            if (IRink(Item).IceThickness <= 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive ice thickness. This is not allowed");
                ShowContinueError("The rink length has been reset to 0.1.");
                IRink(Item).IceThickness = 0.0254;
            }

            IRink(Item).FloodWaterTemp = Numbers(10);
            if (IRink(Item).FloodWaterTemp <= 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive flood water temperature. This is not allowed");
                ShowContinueError("The rink length has been reset to 15.0.");
                IRink(Item).FloodWaterTemp = 15.0;
            }

            IRink(Item).RefrigType = Numbers(11);
            if ((IRink(Item).RefrigType != CaCl2) || (IRink(Item).RefrigType != EG)) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with invalid refrigerant type. This is not allowed");
                ShowContinueError("The refrigerant type has been reset to Ethylene Glycol.");
                IRink(Item).RefrigType = EG;
            }

            IRink(Item).RefrigConc = Numbers(12);
            if ((IRink(Item).RefrigConc < 25.00) || (IRink(Item).RefrigConc > 30.0)) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with invalid refrigerant concentration. This is not allowed");
                ShowContinueError("The refrigerant concentration has been reset to 25%.");
                IRink(Item).RefrigType = 25.0;
            }
        }

        // Obtain user input data for resurfacer
        CurrentModuleObject = "IceRink:Resurfacer";
        for (Item = 1; Item <= NumOfResurfacer; ++Item) {
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
            // General user input
            Resurfacer(Item).Name = Alphas(1);

            Resurfacer(Item).ResurfacingSchedName = Alphas(2);
            if (lAlphaBlanks(2)) {
                Resurfacer(Item).ResurfacingSchedPtr = 0;
            } else {
                Resurfacer(Item).ResurfacingSchedPtr = GetScheduleIndex(Alphas(2));
                if (Resurfacer(Item).ResurfacingSchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " not found for " + Alphas(1));
                    ShowContinueError("Missing " + cAlphaFields(2) + " is " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            Resurfacer(Item).NoOfResurfEvents = Numbers(1);
            if (lNumericBlanks(1)) {
                Resurfacer(Item).NoOfResurfEvents = 1;
            }
            if (Resurfacer(Item).NoOfResurfEvents < 0) {
                ShowSevereError(cNumericFields(1) + "not found for " + Alphas(1));
                ErrorsFound = true;
            }

            Resurfacer(Item).ResurfacingWaterTemp = Numbers(2);
            if (Resurfacer(Item).ResurfacingWaterTemp <= 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive resurfacing water temperature. This is not allowed");
                ShowContinueError("The resurfacing water temperature has been reset to 15.0.");
                Resurfacer(Item).ResurfacingWaterTemp = 15.0;
            }

            Resurfacer(Item).InitWaterTemp = Numbers(3);
            Resurfacer(Item).TankCapacity = Numbers(4);
            if (Resurfacer(Item).TankCapacity <= 0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " was entered with zero or negetive resurfacer tank capacity. This is not allowed");
                ShowContinueError("The resurfacer tank capacity has been reset to 3.0.");
                Resurfacer(Item).ResurfacingWaterTemp = 3.0;
            }
        }

        Alphas.deallocate();
        Numbers.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in input. Preceding conditions cause termination.");
        }

        // Set up the output variables for DRink

        for (Item = 1; Item <= NumOfDirectRefrigSys; ++Item) {
            SetupOutputVariable("Rink floor Cooling rate", OutputProcessor::Unit::W, DRink(Item).CoolPower, "System", "Average", DRink(Item).Name);

            SetupOutputVariable("Rink floor cooling energy",
                                OutputProcessor::Unit::J,
                                DRink(Item).CoolEnergy,
                                "System",
                                "Sum",
                                DRink(Item).Name,
                                _,
                                "ENERGYTRANSFER",
                                _,
                                _,
                                "System");
            SetupOutputVariable("Rink floor cooling fluid energy",
                                OutputProcessor::Unit::J,
                                DRink(Item).CoolEnergy,
                                "System",
                                "Sum",
                                DRink(Item).Name,
                                _,
                                "ENERGYTRANSFER",
                                _,
                                _,
                                "System");
            SetupOutputVariable(
                "Fluid Mass Flow Rate", OutputProcessor::Unit::kg_s, DRink(Item).RefrigMassFlow, "System", "Average", DRink(Item).Name);
            SetupOutputVariable(
                "Refrigerant Inlet Temperature", OutputProcessor::Unit::C, DRink(Item).RefrigInletTemp, "System", "Average", DRink(Item).Name);
            SetupOutputVariable(
                "Refrigerant Outlet Temperature", OutputProcessor::Unit::C, DRink(Item).RefrigOutletTemp, "System", "Average", DRink(Item).Name);
        }

        // Set up the output variables for IRink

        for (Item = 1; Item <= NumOfIndirectRefrigSys; ++Item) {
            SetupOutputVariable("Rink floor Cooling rate", OutputProcessor::Unit::W, IRink(Item).CoolPower, "System", "Average", IRink(Item).Name);

            SetupOutputVariable("Rink floor cooling energy",
                                OutputProcessor::Unit::J,
                                IRink(Item).CoolEnergy,
                                "System",
                                "Sum",
                                IRink(Item).Name,
                                _,
                                "ENERGYTRANSFER",
                                _,
                                _,
                                "System");
            SetupOutputVariable("Rink floor cooling fluid energy",
                                OutputProcessor::Unit::J,
                                IRink(Item).CoolEnergy,
                                "System",
                                "Sum",
                                IRink(Item).Name,
                                _,
                                "ENERGYTRANSFER",
                                _,
                                _,
                                "System");
            SetupOutputVariable(
                "Fluid Mass Flow Rate", OutputProcessor::Unit::kg_s, IRink(Item).RefrigMassFlow, "System", "Average", IRink(Item).Name);
            SetupOutputVariable(
                "Refrigerant Inlet Temperature", OutputProcessor::Unit::C, IRink(Item).RefrigInletTemp, "System", "Average", IRink(Item).Name);
            SetupOutputVariable(
                "Refrigerant Outlet Temperature", OutputProcessor::Unit::C, IRink(Item).RefrigOutletTemp, "System", "Average", IRink(Item).Name);
        }

        // Set output variables for resurfacer

        for (Item = 1; Item <= NumOfResurfacer; ++Item) {
            SetupOutputVariable("Heat load due to resurfacing",
                                OutputProcessor::Unit::J,
                                Resurfacer(Item).ResurfacingHeatLoad,
                                "System",
                                "Average",
                                Resurfacer(Item).Name);
        }
    }

    void InitIndoorIceRink(bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                           int const SysNum,              // Index for the low temperature radiant system under consideration within the derived types
                           int const SystemType,          // Type of radiant system: hydronic, constant flow, or electric
                           bool &InitErrorsFound)
    {
        // Using/Aliasing
        using DataGlobals::AnyPlantInModel;
        using DataGlobals::BeginEnvrnFlag;
        using DataGlobals::NumOfZones;
        using DataPlant::PlantLoop;
        using DataPlant::TypeOf_IceRink_Indoor;
        using DataSurfaces::SurfaceClass_Floor;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const ZeroTol(0.0000001); // Smallest non-zero value allowed
        static std::string const RoutineName("InitIndoorIceRink");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int LoopCounter;
        int SurfNum;
        int SurfFloor;
        static Array1D_bool MyEnvrnFlagDRink;
        static Array1D_bool MyEnvrnFlagIRink;
        static bool MyEnvrnFlagGeneral(true);
        static bool ZoneEquipmentListChecked(false); // True after the Zone Equipment List has been checked for items
        int Loop;
        int ZoneNum;                     // Intermediate variable for keeping track of the zone number
        static bool MyOneTimeFlag(true); // Initialization flag
        static Array1D_bool MyPlantScanFlagDRink;
        static Array1D_bool MyPlantScanFlagIRink;
        Real64 mdot; // local fluid mass flow rate
        Real64 rho;  // local fluid density
        Real64 HeatGainPerPerson;
        Real64 PeopleModifier;
        bool errFlag;

        InitErrorsFound = false;

        if (MyOneTimeFlag) {
            MyEnvrnFlagDRink.allocate(NumOfDirectRefrigSys);
            MyEnvrnFlagIRink.allocate(NumOfIndirectRefrigSys);
            MyPlantScanFlagDRink.allocate(NumOfDirectRefrigSys);
            MyPlantScanFlagIRink.allocate(NumOfIndirectRefrigSys);
            MyPlantScanFlagDRink = true;
            MyPlantScanFlagIRink = true;
            MyEnvrnFlagDRink = true;
            MyEnvrnFlagIRink = true;
            MyOneTimeFlag = false;
        }

        if (FirstTimeInit) {
            ZeroSourceSumHATsurf.dimension(NumOfZones, 0.0);
            QRadSysSrcAvg.dimension(TotSurfaces, 0.0);
            LastQRadSysSrc.dimension(TotSurfaces, 0.0);
            LastSysTimeElapsed.dimension(TotSurfaces, 0.0);

            // Initialize total area for all refrigeration systems
            for (LoopCounter = 1; LoopCounter <= NumOfDirectRefrigSys; ++LoopCounter) {
                for (SurfNum = 1; SurfNum <= DRink(LoopCounter).NumOfSurfaces; ++SurfNum) {
                    if (Surface(DRink(LoopCounter).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                        SurfFloor = DRink(SysNum).SurfacePtrArray(SurfNum);
                        DRink(LoopCounter).TotalSurfaceArea += Surface(DRink(LoopCounter).SurfacePtrArray(SurfFloor)).Area;
                    }
                }
            }

            for (LoopCounter = 1; LoopCounter <= NumOfIndirectRefrigSys; ++LoopCounter) {
                for (SurfNum = 1; SurfNum <= IRink(LoopCounter).NumOfSurfaces; ++SurfNum) {
                    if (Surface(IRink(LoopCounter).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                        SurfFloor = IRink(SysNum).SurfacePtrArray(SurfNum);
                        DRink(LoopCounter).TotalSurfaceArea += Surface(IRink(LoopCounter).SurfacePtrArray(SurfFloor)).Area;
                    }
                }
            }

            FirstTimeInit = false;
        }

        if (SystemType == DirectSystem) {
            if (MyPlantScanFlagDRink(SysNum) && allocated(PlantLoop)) {
                errFlag = false;
                if (DRink(SysNum).RefrigInNode > 0) {
                    ScanPlantLoopsForObject(DRink(SysNum).Name,
                                            TypeOf_IceRink_Indoor,
                                            DRink(SysNum).RefrigLoopNum,
                                            DRink(SysNum).RefrigLoopSide,
                                            DRink(SysNum).RefrigBranchNum,
                                            DRink(SysNum).RefrigCompNum,
                                            errFlag,
                                            _,
                                            _,
                                            _,
                                            DRink(SysNum).RefrigInNode,
                                            _);
                    if (errFlag) {
                        ShowFatalError("InitIndoorIceRink: Program terminated due to previous condition(s).");
                    }
                }
                MyPlantScanFlagDRink(SysNum) = false;
            } else if (MyPlantScanFlagDRink(SysNum) && !AnyPlantInModel) {
                MyPlantScanFlagDRink(SysNum) = false;
            }
        }

        if (SystemType == IndirectSystem) {
            if (MyPlantScanFlagIRink(SysNum) && allocated(PlantLoop)) {
                errFlag = false;
                if (IRink(SysNum).RefrigInNode > 0) {
                    ScanPlantLoopsForObject(IRink(SysNum).Name,
                                            TypeOf_IceRink_Indoor,
                                            IRink(SysNum).RefrigLoopNum,
                                            IRink(SysNum).RefrigLoopSide,
                                            IRink(SysNum).RefrigBranchNum,
                                            IRink(SysNum).RefrigCompNum,
                                            errFlag,
                                            _,
                                            _,
                                            _,
                                            IRink(SysNum).RefrigInNode,
                                            _);
                    if (errFlag) {
                        ShowFatalError("InitIndoorIceRink: Program terminated due to previous condition(s).");
                    }
                }
                MyPlantScanFlagIRink(SysNum) = false;
            } else if (MyPlantScanFlagIRink(SysNum) && !AnyPlantInModel) {
                MyPlantScanFlagIRink(SysNum) = false;
            }
        }

        if (BeginEnvrnFlag && MyEnvrnFlagGeneral) {
            ZeroSourceSumHATsurf = 0.0;
            QRadSysSrcAvg = 0.0;
            LastQRadSysSrc = 0.0;
            LastSysTimeElapsed = 0.0;
            LastTimeStepSys = 0.0;
            MyEnvrnFlagGeneral = false;
        }
        if (!BeginEnvrnFlag) MyEnvrnFlagGeneral = true;

        if (SystemType == DirectSystem) {
            if (BeginEnvrnFlag && MyEnvrnFlagDRink(SysNum)) {

                DRink(SysNum).RefrigInletTemp = 0.0;
                DRink(SysNum).RefrigOutletTemp = 0.0;
                DRink(SysNum).RefrigMassFlow = 0.0;
                DRink(SysNum).CoolPower = 0.0;
                DRink(SysNum).CoolEnergy = 0.0;

                if (!MyPlantScanFlagDRink(SysNum)) {
                    if (DRink(SysNum).RefrigInNode > 0) {
                        InitComponentNodes(0.0,
                                           DRink(SysNum).MaxRefrigMassFlow,
                                           DRink(SysNum).RefrigInNode,
                                           DRink(SysNum).RefrigOutNode,
                                           DRink(SysNum).RefrigLoopNum,
                                           DRink(SysNum).RefrigLoopSide,
                                           DRink(SysNum).RefrigBranchNum,
                                           DRink(SysNum).RefrigCompNum);
                    }
                }
                MyEnvrnFlagDRink(SysNum) = false;
            }
        }
        if (!BeginEnvrnFlag && SystemType == DirectSystem) MyEnvrnFlagDRink(SysNum) = true;

        if (SystemType == IndirectSystem) {
            if (BeginEnvrnFlag && MyEnvrnFlagIRink(SysNum)) {

                IRink(SysNum).RefrigInletTemp = 0.0;
                IRink(SysNum).RefrigOutletTemp = 0.0;
                IRink(SysNum).RefrigMassFlow = 0.0;
                IRink(SysNum).CoolPower = 0.0;
                IRink(SysNum).CoolEnergy = 0.0;

                if (!MyPlantScanFlagIRink(SysNum)) {
                    if (IRink(SysNum).RefrigInNode > 0) {
                        InitComponentNodes(0.0,
                                           IRink(SysNum).MaxRefrigMassFlow,
                                           IRink(SysNum).RefrigInNode,
                                           IRink(SysNum).RefrigOutNode,
                                           IRink(SysNum).RefrigLoopNum,
                                           IRink(SysNum).RefrigLoopSide,
                                           IRink(SysNum).RefrigBranchNum,
                                           IRink(SysNum).RefrigCompNum);
                    }
                }
                MyEnvrnFlagIRink(SysNum) = false;
            }
        }
        if (!BeginEnvrnFlag && SystemType == IndirectSystem) MyEnvrnFlagIRink(SysNum) = true;

        if (BeginTimeStepFlag && FirstHVACIteration) {
            {
                auto const SELECT_CASE_var(SystemType);

                if (SELECT_CASE_var == DirectSystem) {
                    ZoneNum = DRink(SysNum).ZonePtr;
                    ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum); // Set this to figure what partial part of the load the radiant system meets
                    for (SurfNum = 1; SurfNum < DRink(SysNum).NumOfSurfaces; ++SurfNum) {
                        if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                            SurfFloor = DRink(SysNum).SurfacePtrArray(SurfNum);
                        }
                    }
                    QRadSysSrcAvg(SurfFloor) = 0.0;      // Initialize this variable to zero (refrigeration system defaults to off)
                    LastQRadSysSrc(SurfFloor) = 0.0;     // At the start of a time step, reset to zero so average calculation can begin again
                    LastSysTimeElapsed(SurfFloor) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
                    LastTimeStepSys(SurfFloor) = 0.0;    // At the start of a time step, reset to zero so average calculation can begin again
                } else if (SystemType == IndirectSystem) {
                    ZoneNum = IRink(SysNum).ZonePtr;
                    ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum); // Set this to figure what partial part of the load the radiant system meets
                    for (SurfNum = 1; SurfNum < IRink(SysNum).NumOfSurfaces; ++SurfNum) {
                        if (Surface(IRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                            SurfFloor = IRink(SysNum).SurfacePtrArray(SurfNum);
                        }
                    }
                    QRadSysSrcAvg(SurfFloor) = 0.0;      // Initialize this variable to zero (refrigeration system defaults to off)
                    LastQRadSysSrc(SurfFloor) = 0.0;     // At the start of a time step, reset to zero so average calculation can begin again
                    LastSysTimeElapsed(SurfFloor) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
                    LastTimeStepSys(SurfFloor) = 0.0;    // At the start of a time step, reset to zero so average calculation can begin again
                } else {

                    ShowSevereError("Refrigeration system entered without specification of type: Direct or Indirect?");
                    ShowContinueError("Occurs in Refrigeration System=" + DRink(SysNum).Name);
                    ShowFatalError("Preceding condition causes termination.");
                }
            }
        }

        {
            auto const SELECT_CASE_var(SystemType);

            if (SELECT_CASE_var == DirectSystem) {

                // Initialize the appropriate node data
                mdot = 0.0;
                SetComponentFlowRate(mdot,
                                     DRink(SysNum).RefrigInNode,
                                     DRink(SysNum).RefrigOutNode,
                                     DRink(SysNum).RefrigLoopNum,
                                     DRink(SysNum).RefrigLoopSide,
                                     DRink(SysNum).RefrigBranchNum,
                                     DRink(SysNum).RefrigCompNum);
            } else if (SELECT_CASE_var == IndirectSystem) {
                // Initialize the appropriate node data
                mdot = 0.0;
                SetComponentFlowRate(mdot,
                                     IRink(SysNum).RefrigInNode,
                                     IRink(SysNum).RefrigOutNode,
                                     IRink(SysNum).RefrigLoopNum,
                                     IRink(SysNum).RefrigLoopSide,
                                     IRink(SysNum).RefrigBranchNum,
                                     IRink(SysNum).RefrigCompNum);
            }
        }

        OperatingMode = NotOperating;

        // get the schedule values for different scheduled parameters
        if (SystemType == DirectSystem) {
            DRink(SysNum).RefrigSetptTemp = GetCurrentScheduleValue(DRink(SysNum).RefrigSetptSchedPtr);
            DRink(SysNum).IceSetptTemp = GetCurrentScheduleValue(DRink(SysNum).IceSetptSchedPtr);

            // Determine the current heat gain from people
            if (DRink(SysNum).PeopleHeatGainSchedPtr > 0) {
                HeatGainPerPerson = GetCurrentScheduleValue(DRink(SysNum).PeopleHeatGainSchedPtr);
                if (HeatGainPerPerson < 0.0) {
                    ShowWarningError(RoutineName + ": Ice Rink =\"" + DRink(SysNum).Name + " Heat Gain Schedule =\"" +
                                     DRink(SysNum).PeopleHeatGainSchedName + " has a negative value.  This is not allowed.");
                    ShowContinueError("The heat gain per person has been reset to zero.");
                    HeatGainPerPerson = 0.0;
                }
                if (DRink(SysNum).PeopleSchedPtr > 0) {
                    PeopleModifier = GetCurrentScheduleValue(DRink(SysNum).PeopleSchedPtr);
                    if (PeopleModifier < 0.0) {
                        ShowWarningError(RoutineName + ": Ice Rink =\"" + DRink(SysNum).Name + " People Schedule =\"" +
                                         DRink(SysNum).PeopleSchedName + " has a negative value.  This is not allowed.");
                        ShowContinueError("The number of people has been reset to zero.");
                        PeopleModifier = 0.0;
                    }
                } else { // no people schedule entered--assume that full number always present
                    PeopleModifier = 1.0;
                }
            } else { // no heat gain schedule added--assume a zero value for Heat Gain per Person and no people present
                HeatGainPerPerson = 0.0;
                PeopleModifier = 0.0;
            }
            DRink(SysNum).PeopleHeatGain = PeopleModifier * HeatGainPerPerson * DRink(SysNum).MaxNumOfPeople;
        } else if (SystemType == IndirectSystem) {
            IRink(SysNum).RefrigSetptTemp = GetCurrentScheduleValue(IRink(SysNum).RefrigSetptSchedPtr);
            IRink(SysNum).IceSetptTemp = GetCurrentScheduleValue(IRink(SysNum).IceSetptSchedPtr);

            // Determine the current heat gain from people
            if (IRink(SysNum).PeopleHeatGainSchedPtr > 0) {
                HeatGainPerPerson = GetCurrentScheduleValue(IRink(SysNum).PeopleHeatGainSchedPtr);
                if (HeatGainPerPerson < 0.0) {
                    ShowWarningError(RoutineName + ": Ice Rink =\"" + IRink(SysNum).Name + " Heat Gain Schedule =\"" +
                                     IRink(SysNum).PeopleHeatGainSchedName + " has a negative value.  This is not allowed.");
                    ShowContinueError("The heat gain per person has been reset to zero.");
                    HeatGainPerPerson = 0.0;
                }
                if (IRink(SysNum).PeopleSchedPtr > 0) {
                    PeopleModifier = GetCurrentScheduleValue(IRink(SysNum).PeopleSchedPtr);
                    if (PeopleModifier < 0.0) {
                        ShowWarningError(RoutineName + ": Ice Rink =\"" + IRink(SysNum).Name + " People Schedule =\"" +
                                         IRink(SysNum).PeopleSchedName + " has a negative value.  This is not allowed.");
                        ShowContinueError("The number of people has been reset to zero.");
                        PeopleModifier = 0.0;
                    }
                } else { // no people schedule entered--assume that full number always present
                    PeopleModifier = 1.0;
                }
            } else { // no heat gain schedule added--assume a zero value for Heat Gain per Person and no people present
                HeatGainPerPerson = 0.0;
                PeopleModifier = 0.0;
            }
            IRink(SysNum).PeopleHeatGain = PeopleModifier * HeatGainPerPerson * IRink(SysNum).MaxNumOfPeople;
        }
    }

    void IceRinkFreezing(Real64 &FreezingLoad, // Freezing load
                         int const SysNum,     // Index to refrigeration system
                         int const SystemType  // Type of system (Direct of Indirect)
    )
    {
        // Using/Aliasing
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using ScheduleManager::GetCurrentScheduleValue;
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static std::string const RoutineName("IceRinkFreezing");
        Real64 SetPointTemp; // temperature "goal" for the radiant system [Celsius]
        Real64 Length;       // Length of ice rink
        Real64 Width;        // Width of ice rink
        Real64 Depth;        // Thickness of ice
        Real64 Volume;       // Volume of ice structure
        Real64 CpWater;      // Specific heat of water
        Real64 RhoWater;     // Density of water
        Real64 QFusion(333550.00);
        Real64 CpIce(2108.00);
        Real64 QFreezing;
        Real64 FloodWaterTemp;

        if (SystemType == DirectSystem) {
            SetPointTemp = DRink(SysNum).IceSetptTemp;
            FloodWaterTemp = DRink(SysNum).FloodWaterTemp;
            RhoWater = GetDensityGlycol(fluidNameWater, FloodWaterTemp, DRink(SysNum).WaterIndex, RoutineName);
            CpWater = GetSpecificHeatGlycol(fluidNameWater, FloodWaterTemp, DRink(SysNum).WaterIndex, RoutineName);
            Length = DRink(SysNum).LengthRink;
            Width = DRink(SysNum).WidthRink;
            Depth = DRink(SysNum).IceThickness;
            Volume = Length * Width * Depth;
        } else if (SystemType == IndirectSystem) {
            SetPointTemp = IRink(SysNum).IceSetptTemp;
            FloodWaterTemp = IRink(SysNum).FloodWaterTemp;
            RhoWater = GetDensityGlycol(fluidNameWater, FloodWaterTemp, IRink(SysNum).WaterIndex, RoutineName);
            CpWater = GetSpecificHeatGlycol(fluidNameWater, FloodWaterTemp, IRink(SysNum).WaterIndex, RoutineName);
            Length = IRink(SysNum).LengthRink;
            Width = IRink(SysNum).WidthRink;
            Depth = IRink(SysNum).IceThickness;
            Volume = Length * Width * Depth;
        }

        QFreezing = RhoWater * Volume * ((CpWater * FloodWaterTemp) + (QFusion) - (CpIce * SetPointTemp));

        FreezingLoad = QFreezing;
    }

    void IceRinkResurfacer(Real64 &ResurfacerHeatLoad, // Heat load due to resurfacing events(J)
                           int const SysNum,           // Index to the refrigeration system
                           int const SystemType,       // Type of refrigeration system
                           int const MachineNum        // Number of resurfacers working
    )
    {
        // Using/Aliasing
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using ScheduleManager::GetCurrentScheduleValue;

        Real64 QResurfacing;  // Heat input(KJ) to the rink due to resurfacing events
        Real64 EHeatingWater; // Electric energy(KJ) required to heat water
        Real64 QHumidity;     // Heat input(J) to the ice rink due to humidity change during resurfacing events
        Real64 CpWater;
        Real64 RhoWater;
        Real64 QFusion(333550.00);
        Real64 CpIce(2108.00);
        Real64 MolarMassWater(18.015);
        Real64 T_air_preResurfacing;
        Real64 T_air_postResurfacing;
        Real64 RH_air_preResurfacing;
        Real64 RH_air_postResurfacing;
        Real64 InitResurfWaterTemp;
        Real64 ResurfacerTank_capacity;
        Real64 ResurfacingWaterTemp;
        int NoOfResurfacingEvents;
        Real64 IceSurfaceTemperature;
        Real64 VolumeRink;
        Real64 DeltaT_ice;
        Real64 DeltaAH_ice;
        Real64 AH_preResurfacing;
        Real64 AH_postResurfacing;
        static std::string const RoutineName("IceRinkResurfacer");

        InitResurfWaterTemp = Resurfacer(MachineNum).InitWaterTemp;
        ResurfacerTank_capacity = Resurfacer(MachineNum).TankCapacity;
        if (Resurfacer(MachineNum).ResurfacingSchedPtr > 0) {
            NoOfResurfacingEvents = GetCurrentScheduleValue(Resurfacer(MachineNum).ResurfacingSchedPtr);
        }
        ResurfacingWaterTemp = Resurfacer(MachineNum).ResurfacingWaterTemp;
        NoOfResurfacingEvents = Resurfacer(MachineNum).NoOfResurfEvents;
        if (SystemType == DirectSystem) {

            IceSurfaceTemperature = DRink(SysNum).IceSetptTemp;
            RhoWater = GetDensityGlycol(fluidNameWater, Resurfacer(MachineNum).ResurfacingWaterTemp, Resurfacer(MachineNum).GlycolIndex, RoutineName);
            CpWater =
                GetSpecificHeatGlycol(fluidNameWater, Resurfacer(MachineNum).ResurfacingWaterTemp, Resurfacer(MachineNum).GlycolIndex, RoutineName);
            QResurfacing = NoOfResurfacingEvents * 0.001 * RhoWater * ResurfacerTank_capacity *
                           ((CpWater * Resurfacer(MachineNum).ResurfacingWaterTemp) + (QFusion) - (CpIce * IceSurfaceTemperature));
            EHeatingWater =
                0.001 * ResurfacerTank_capacity * RhoWater * CpWater * (Resurfacer(MachineNum).ResurfacingWaterTemp - InitResurfWaterTemp);

            T_air_preResurfacing = IceSurfaceTemperature;
            T_air_postResurfacing = Resurfacer(MachineNum).ResurfacingWaterTemp;

            // Assuming the RH of air above ice surface before and after resurfacing
            // event is 0% and 100% respectively.

            RH_air_preResurfacing = 0;
            RH_air_postResurfacing = 1;
            DeltaT_ice = abs(IceSurfaceTemperature - Resurfacer(MachineNum).ResurfacingWaterTemp);
            VolumeRink = DRink(SysNum).LengthRink * DRink(SysNum).WidthRink * DRink(SysNum).DepthRink;

            AH_preResurfacing =
                ((6.112 * exp((17.67 * T_air_preResurfacing) / (T_air_preResurfacing + 243.5)) * RH_air_preResurfacing * MolarMassWater) /
                 (100 * 0.08314 * (273.15 + T_air_preResurfacing))) *
                (1 / RhoWater);
            AH_postResurfacing =
                ((6.112 * exp((17.67 * T_air_postResurfacing) / (T_air_postResurfacing + 243.5)) * RH_air_postResurfacing * MolarMassWater) /
                 (100 * 0.08314 * (273.15 + T_air_postResurfacing))) *
                (1 / RhoWater);
            DeltaAH_ice = abs(AH_preResurfacing - AH_postResurfacing);
            QHumidity = NoOfResurfacingEvents * DeltaAH_ice * VolumeRink * DeltaT_ice * CpWater;

        } else if (SystemType == IndirectSystem) {

            IceSurfaceTemperature = IRink(SysNum).IceSetptTemp;
            RhoWater = GetDensityGlycol(fluidNameWater, Resurfacer(MachineNum).ResurfacingWaterTemp, Resurfacer(MachineNum).GlycolIndex, RoutineName);
            CpWater =
                GetSpecificHeatGlycol(fluidNameWater, Resurfacer(MachineNum).ResurfacingWaterTemp, Resurfacer(MachineNum).GlycolIndex, RoutineName);
            QResurfacing = NoOfResurfacingEvents * 0.001 * RhoWater * ResurfacerTank_capacity *
                           ((CpWater * Resurfacer(MachineNum).ResurfacingWaterTemp) + (QFusion) - (CpIce * IceSurfaceTemperature));
            EHeatingWater =
                0.001 * ResurfacerTank_capacity * RhoWater * CpWater * (Resurfacer(MachineNum).ResurfacingWaterTemp - InitResurfWaterTemp);

            T_air_preResurfacing = IceSurfaceTemperature;
            T_air_postResurfacing = Resurfacer(MachineNum).ResurfacingWaterTemp;
            RH_air_preResurfacing = 0;
            RH_air_postResurfacing = 1;
            DeltaT_ice = abs(IceSurfaceTemperature - Resurfacer(MachineNum).ResurfacingWaterTemp);
            VolumeRink = IRink(SysNum).LengthRink * IRink(SysNum).WidthRink * IRink(SysNum).DepthRink;

            AH_preResurfacing =
                ((6.112 * exp((17.67 * T_air_preResurfacing) / (T_air_preResurfacing + 243.5)) * RH_air_preResurfacing * MolarMassWater) /
                 (100 * 0.08314 * (273.15 + T_air_preResurfacing))) *
                (1 / RhoWater);
            AH_postResurfacing =
                ((6.112 * exp((17.67 * T_air_postResurfacing) / (T_air_postResurfacing + 243.5)) * RH_air_postResurfacing * MolarMassWater) /
                 (100 * 0.08314 * (273.15 + T_air_postResurfacing))) *
                (1 / RhoWater);
            DeltaAH_ice = abs(AH_preResurfacing - AH_postResurfacing);
            QHumidity = NoOfResurfacingEvents * DeltaAH_ice * VolumeRink * DeltaT_ice * CpWater;
        }
        ResurfacerHeatLoad = QResurfacing + QHumidity;
    }

    Real64 CalcEffectivenessDRink(int const SysNum,
                                  Real64 const Temperature,    // Temperature of refrigerant entering the floor radiant system, in C)
                                  Real64 const RefrigMassFlow, // Mass flow rate of refrigerant in the floor radiant system, in kg/s
                                  Real64 const NumCircs,       // Number of fluid circuits in this surface
                                  Real64 const TubeLength,     // Length of tubing in the floor radiant system, in m
                                  Real64 const TubeDiameter)   // Inside diameter of the tubing in the floor radiant system, in m
    {
        // Using/Aliasing
        using DataGlobals::Pi;
        using DataPlant::PlantLoop;
        using FluidProperties::GetSpecificHeatGlycol;

        Real64 Effectiveness;
        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const MaxLaminarRe(2300.0); // Maximum Reynolds number for laminar flow
        int const NumOfPropDivisions(11);
        Real64 const MaxExpPower(50.0); // Maximum power after which EXP argument would be zero for DP variables
        static Array1D<Real64> const Temps(NumOfPropDivisions, {-10.00, -9.00, -8.00, -7.00, -6.00, -5.00, -4.00, -3.00, -2.00, -1.00, 0.00});
        static Array1D<Real64> const Mu(
            NumOfPropDivisions,
            {0.0001903, 0.0001881, 0.000186, 0.0001839, 0.0001818, 0.0001798, 0.0001778, 0.0001759, 0.000174, 0.0001721, 0.0001702});

        static Array1D<Real64> const Conductivity(NumOfPropDivisions,
                                                  {0.5902, 0.5871, 0.584, 0.5809, 0.5778, 0.5747, 0.5717, 0.5686, 0.5655, 0.5625, 0.5594});
        static Array1D<Real64> const Pr(NumOfPropDivisions, {1.471, 1.464, 1.456, 1.449, 1.442, 1.436, 1.429, 1.423, 1.416, 1.41, 1.404});

        static std::string const RoutineName("CalcEffectivenessDRink");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Index;
        Real64 InterpFrac;
        Real64 NuD;
        Real64 ReD;
        Real64 NTU;
        Real64 CpNH3(0.0);
        Real64 Kactual;
        Real64 MUactual;
        Real64 PRactual;

        // First find out where we are in the range of temperatures
        Index = 1;
        while (Index <= NumOfPropDivisions) {
            if (Temperature < Temps(Index)) break; // DO loop
            ++Index;
        }

        // Initialize thermal properties of water
        if (Index == 1) {
            MUactual = Mu(Index);
            Kactual = Conductivity(Index);
            PRactual = Pr(Index);
        } else if (Index > NumOfPropDivisions) {
            Index = NumOfPropDivisions;
            MUactual = Mu(Index);
            Kactual = Conductivity(Index);
            PRactual = Pr(Index);
        } else {
            InterpFrac = (Temperature - Temps(Index - 1)) / (Temps(Index) - Temps(Index - 1));
            MUactual = Mu(Index - 1) + InterpFrac * (Mu(Index) - Mu(Index - 1));
            Kactual = Conductivity(Index - 1) + InterpFrac * (Conductivity(Index) - Conductivity(Index - 1));
            PRactual = Pr(Index - 1) + InterpFrac * (Pr(Index) - Pr(Index - 1));
        }

        CpNH3 = GetSpecificHeatGlycol(
            PlantLoop(DRink(SysNum).RefrigLoopNum).FluidName, Temperature, PlantLoop(DRink(SysNum).RefrigLoopNum).FluidIndex, RoutineName);
        // Claculate the reynold's number
        ReD = 4.0 * RefrigMassFlow / (Pi * MUactual * TubeDiameter * NumCircs);

        if (ReD >= MaxLaminarRe) { // Turbulent flow --> use Colburn equation
            NuD = 0.023 * std::pow(ReD, 0.8) * std::pow(PRactual, 1.0 / 3.0);
        } else { // Laminar flow --> use constant surface temperature relation
            NuD = 3.66;
        }

        NTU = Pi * Kactual * NuD * TubeLength / (RefrigMassFlow * CpNH3);
        if (NTU > MaxExpPower) {
            Effectiveness = 1.0;
        } else {
            Effectiveness = 1.0 - std::exp(-NTU);
        }

        return Effectiveness;
    }

    Real64
    CalcEffectivenessIRink(int const SysNum,
                           Real64 const Temperature,    // Temperature of refrigerant entering the floor radiant system, in C)
                           Real64 const RefrigMassFlow, // Mass flow rate of refrigerant in the floor radiant system, in kg/s
                           Real64 const NumCircs,       // Number of fluid circuits in this surface
                           Real64 const TubeLength,     // Length of tubing in the floor radiant system, in m
                           Real64 const TubeDiameter,   // Inside diameter of the tubing in the floor radiant system, in m
                           Real64 const RefrigType,     // Refrigerant used in the radiant system: Ethylene Glycol(EG) or Cslcium Chloride(CaCl2)
                           Real64 const Concentration)  // Concentration of the brine(refrigerant) in the radiant system (allowed range 25% to 30%)
    {
        // Using/Aliasing
        using DataGlobals::Pi;
        using DataPlant::PlantLoop;
        using FluidProperties::GetSpecificHeatGlycol;

        Real64 Effectiveness;
        Real64 EpsMdotCp;

        Real64 const MaxLaminarRe(2300.0); // Maximum Reynolds number for laminar flow
        int const NumOfTempDivisions(11);  // Number of Temperature divisions (i.e the number of temperature data points)
        Real64 const MaxExpPower(50.0);    // Maximum power after which EXP argument would be zero for DP variables

        // Number of Data points for brines ( Calcium Chloride and Ethylene Glycol solution
        static Array1D<Real64> const Temperatures(NumOfTempDivisions, {-10.00, -9.00, -8.00, -7.00, -6.00, -5.00, -4.00, -3.00, -2.00, -1.00, 0.00});

        // Properties of Calcium chloride  solution at 25% - 30% concentration
        // Conductivity (Watt/m-C)
        static Array1D<Real64> const ConductivityCacl2_C25(NumOfTempDivisions,
                                                           {0.5253, 0.5267, 0.5281, 0.5296, 0.531, 0.5324, 0.5338, 0.5352, 0.5366, 0.5381, 0.5395});
        static Array1D<Real64> const ConductivityCacl2_C26(NumOfTempDivisions,
                                                           {0.524, 0.5254, 0.5268, 0.5283, 0.5297, 0.5311, 0.5325, 0.5339, 0.5353, 0.5367, 0.5381});
        static Array1D<Real64> const ConductivityCacl2_C27(NumOfTempDivisions,
                                                           {0.5227, 0.5241, 0.5255, 0.5269, 0.5284, 0.5298, 0.5312, 0.5326, 0.534, 0.5354, 0.5368});
        static Array1D<Real64> const ConductivityCacl2_C28(NumOfTempDivisions,
                                                           {0.5214, 0.5228, 0.5242, 0.5256, 0.527, 0.5285, 0.5299, 0.5313, 0.5327, 0.5341, 0.5355});
        static Array1D<Real64> const ConductivityCacl2_C29(NumOfTempDivisions,
                                                           {0.5201, 0.5215, 0.5229, 0.5243, 0.5258, 0.5272, 0.5286, 0.53, 0.5314, 0.5328, 0.5342});
        static Array1D<Real64> const ConductivityCacl2_C30(NumOfTempDivisions,
                                                           {0.5189, 0.5203, 0.5217, 0.5231, 0.5245, 0.5259, 0.5273, 0.5287, 0.5301, 0.5315, 0.5329});
        // Viscousity (kg/m.s)
        static Array1D<Real64> const MuCacl2_C25(
            NumOfTempDivisions, {0.00553, 0.005353, 0.005184, 0.005023, 0.004869, 0.004722, 0.004582, 0.004447, 0.004319, 0.004197, 0.004079});
        static Array1D<Real64> const MuCacl2_C26(
            NumOfTempDivisions, {0.005854, 0.005665, 0.005485, 0.005314, 0.005151, 0.004995, 0.004847, 0.004705, 0.004569, 0.00444, 0.004316});
        static Array1D<Real64> const MuCacl2_C27(
            NumOfTempDivisions, {0.006217, 0.006015, 0.005823, 0.005641, 0.005467, 0.005301, 0.005143, 0.004992, 0.004848, 0.00471, 0.004579});
        static Array1D<Real64> const MuCacl2_C28(
            NumOfTempDivisions, {0.006627, 0.00641, 0.006204, 0.006007, 0.005821, 0.005643, 0.005474, 0.005313, 0.005159, 0.005012, 0.004872});
        static Array1D<Real64> const MuCacl2_C29(
            NumOfTempDivisions, {0.007093, 0.006858, 0.006635, 0.006423, 0.006221, 0.00603, 0.005848, 0.005674, 0.005509, 0.005351, 0.0052});
        static Array1D<Real64> const MuCacl2_C30(
            NumOfTempDivisions, {0.007627, 0.00737, 0.007127, 0.006896, 0.006677, 0.006469, 0.006272, 0.006084, 0.005905, 0.005734, 0.005572});
        // Prandlt Number
        static Array1D<Real64> const PrCacl2_C25(NumOfTempDivisions, {29.87, 28.87, 27.91, 27.00, 26.13, 25.31, 24.52, 23.76, 23.04, 22.35, 21.69});
        static Array1D<Real64> const PrCacl2_C26(NumOfTempDivisions, {31.35, 30.29, 29.28, 28.32, 27.41, 26.54, 25.71, 24.92, 24.16, 23.44, 22.75});
        static Array1D<Real64> const PrCacl2_C27(NumOfTempDivisions, {33.02, 31.90, 30.83, 29.82, 28.85, 27.93, 27.05, 26.22, 25.42, 24.66, 23.93});
        static Array1D<Real64> const PrCacl2_C28(NumOfTempDivisions, {34.93, 33.73, 32.59, 31.51, 30.48, 29.50, 28.57, 27.68, 26.83, 26.03, 25.26});
        static Array1D<Real64> const PrCacl2_C29(NumOfTempDivisions, {37.10, 35.81, 34.58, 33.42, 32.32, 31.27, 30.27, 29.33, 28.42, 27.56, 26.74});
        static Array1D<Real64> const PrCacl2_C30(NumOfTempDivisions, {39.59, 38.19, 36.86, 35.60, 34.41, 33.28, 32.20, 31.18, 30.21, 29.29, 28.41});

        // Specific heat (J/kg.K)
        static Array1D<Real64> const CpCacl2_C25(NumOfTempDivisions,
                                                 {2837.00, 2840.00, 2844.00, 2847.00, 2850.00, 2853.00, 2856.00, 2859.00, 2863.00, 2866.00, 2869.00});
        static Array1D<Real64> const CpCacl2_C26(NumOfTempDivisions,
                                                 {2806.00, 2809.00, 2812.00, 2815.00, 2819.00, 2822.00, 2825.00, 2828.00, 2831.00, 2834.00, 2837.00});
        static Array1D<Real64> const CpCacl2_C27(NumOfTempDivisions,
                                                 {2777.00, 2780.00, 2783.00, 2786.00, 2789.00, 2792.00, 2794.00, 2797.00, 2800.00, 2803.00, 2806.00});
        static Array1D<Real64> const CpCacl2_C28(NumOfTempDivisions,
                                                 {2748.00, 2751.00, 2754.00, 2757.00, 2760.00, 2762.00, 2765.00, 2768.00, 2771.00, 2774.00, 2776.00});
        static Array1D<Real64> const CpCacl2_C29(NumOfTempDivisions,
                                                 {2721.00, 2723.00, 2726.00, 2729.00, 2731.00, 2734.00, 2736.00, 2739.00, 2742.00, 2744.00, 2747.00});
        static Array1D<Real64> const CpCacl2_C30(NumOfTempDivisions,
                                                 {2693.00, 2696.00, 2698.00, 2700.00, 2703.00, 2705.00, 2708.00, 2710.00, 2712.00, 2715.00, 2717.00});

        // Properties of Ethylene Glycol solution at 25%-30% concentration
        // Conductivity (Watt/m-C)
        static Array1D<Real64> const ConductivityEG_C25(NumOfTempDivisions,
                                                        {0.4538, 0.4549, 0.456, 0.4571, 0.4582, 0.4593, 0.4604, 0.4615, 0.4626, 0.4637, 0.4648});
        static Array1D<Real64> const ConductivityEG_C26(NumOfTempDivisions,
                                                        {0.4502, 0.4513, 0.4524, 0.4535, 0.4546, 0.4557, 0.4567, 0.4578, 0.4589, 0.4599, 0.461});
        static Array1D<Real64> const ConductivityEG_C27(NumOfTempDivisions,
                                                        {0.4467, 0.4478, 0.4488, 0.4499, 0.4509, 0.452, 0.453, 0.4541, 0.4551, 0.4562, 0.4572});
        static Array1D<Real64> const ConductivityEG_C28(NumOfTempDivisions,
                                                        {0.4432, 0.4442, 0.4452, 0.4463, 0.4473, 0.4483, 0.4493, 0.4504, 0.4514, 0.4524, 0.4534});
        static Array1D<Real64> const ConductivityEG_C29(NumOfTempDivisions,
                                                        {0.4397, 0.4407, 0.4417, 0.4427, 0.4437, 0.4447, 0.4457, 0.4467, 0.4477, 0.4487, 0.4497});
        static Array1D<Real64> const ConductivityEG_C30(NumOfTempDivisions,
                                                        {0.4362, 0.4371, 0.4381, 0.4391, 0.4401, 0.4411, 0.442, 0.443, 0.444, 0.445, 0.4459});
        // Viscousity
        static Array1D<Real64> const MuEG_C25(
            NumOfTempDivisions, {0.005531, 0.0053, 0.005082, 0.004876, 0.00468, 0.004494, 0.004318, 0.004151, 0.003992, 0.003841, 0.003698});
        static Array1D<Real64> const MuEG_C26(
            NumOfTempDivisions, {0.005713, 0.005474, 0.005248, 0.005033, 0.00483, 0.004637, 0.004454, 0.004281, 0.004116, 0.003959, 0.003811});
        static Array1D<Real64> const MuEG_C27(
            NumOfTempDivisions, {0.005902, 0.005654, 0.005418, 0.005195, 0.004984, 0.004784, 0.004594, 0.004414, 0.004244, 0.004081, 0.003927});
        static Array1D<Real64> const MuEG_C28(
            NumOfTempDivisions, {0.006098, 0.005839, 0.005595, 0.005363, 0.005144, 0.004936, 0.004739, 0.004552, 0.004375, 0.004207, 0.004047});
        static Array1D<Real64> const MuEG_C29(
            NumOfTempDivisions, {0.006299, 0.006031, 0.005776, 0.005536, 0.005308, 0.005093, 0.004888, 0.004694, 0.004511, 0.004336, 0.004171});
        static Array1D<Real64> const MuEG_C30(
            NumOfTempDivisions, {0.006508, 0.006228, 0.005964, 0.005715, 0.005478, 0.005254, 0.005042, 0.004841, 0.00465, 0.004469, 0.004298});
        // Prandlt Number
        static Array1D<Real64> const PrEG_C25(NumOfTempDivisions, {45.57, 43.59, 41.72, 39.95, 38.28, 36.70, 35.20, 33.77, 32.43, 31.15, 29.93});
        static Array1D<Real64> const PrEG_C26(NumOfTempDivisions, {47.17, 45.11, 43.17, 41.34, 39.60, 37.95, 36.40, 34.92, 33.52, 32.19, 30.94});
        static Array1D<Real64> const PrEG_C27(NumOfTempDivisions, {48.82, 46.69, 44.67, 42.76, 40.96, 39.25, 37.64, 36.10, 34.65, 33.27, 31.97});
        static Array1D<Real64> const PrEG_C28(NumOfTempDivisions, {50.53, 48.31, 46.22, 44.24, 42.36, 40.59, 38.91, 37.32, 35.81, 34.39, 33.03});
        static Array1D<Real64> const PrEG_C29(NumOfTempDivisions, {52.29, 49.99, 47.81, 45.76, 43.81, 41.97, 40.23, 38.58, 37.01, 35.53, 34.13});
        static Array1D<Real64> const PrEG_C30(NumOfTempDivisions, {54.12, 51.72, 49.46, 47.32, 45.30, 43.39, 41.58, 39.87, 38.25, 36.71, 35.25});
        // Specific heat (J/kg.K)
        static Array1D<Real64> const CpEG_C25(NumOfTempDivisions,
                                              {3739.00, 3741.00, 3744.00, 3746.00, 3748.00, 3751.00, 3753.00, 3756.00, 3758.00, 3760.00, 3763.00});
        static Array1D<Real64> const CpEG_C26(NumOfTempDivisions,
                                              {3717.00, 3719.00, 3722.00, 3725.00, 3727.00, 3730.00, 3732.00, 3735.00, 3737.00, 3740.00, 3742.00});
        static Array1D<Real64> const CpEG_C27(NumOfTempDivisions,
                                              {3695.00, 3698.00, 3700.00, 3703.00, 3706.00, 3708.00, 3711.00, 3714.00, 3716.00, 3719.00, 3722.00});
        static Array1D<Real64> const CpEG_C28(NumOfTempDivisions,
                                              {3672.00, 3675.00, 3678.00, 3681.00, 3684.00, 3687.00, 3689.00, 3692.00, 3695.00, 3698.00, 3701.00});
        static Array1D<Real64> const CpEG_C29(NumOfTempDivisions,
                                              {3650.00, 3653.00, 3656.00, 3659.00, 3662.00, 3665.00, 3668.00, 3671.00, 3674.00, 3677.00, 3680.00});
        static Array1D<Real64> const CpEG_C30(NumOfTempDivisions,
                                              {3627.00, 3630.00, 3633.00, 3636.00, 3640.00, 3643.00, 3646.00, 3649.00, 3652.00, 3655.00, 3658.00});

        static std::string const RoutineName("CalcEffectivenessIRink");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Index;
        Real64 InterpFrac;
        Real64 NuD;
        Real64 ReD;
        Real64 NTU;
        Real64 Kactual;
        Real64 MUactual;
        Real64 PRactual;
        Real64 Cpactual;

        // First find out where we are in the range of temperatures
        Index = 1;
        while (Index <= NumOfTempDivisions) {
            if (Temperature < Temperatures(Index)) break; // DO loop
            ++Index;
        }

        if (RefrigType == CaCl2) {
            // Initialize thermal properties of Calcium Chloride Solution
            if (Concentration == 25.00) {
                if (Index == 1) {
                    MUactual = MuCacl2_C25(Index);
                    Kactual = ConductivityCacl2_C25(Index);
                    PRactual = PrCacl2_C25(Index);
                    Cpactual = CpCacl2_C25(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuCacl2_C25(Index);
                    Kactual = ConductivityCacl2_C25(Index);
                    PRactual = PrCacl2_C25(Index);
                    Cpactual = CpCacl2_C25(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuCacl2_C25(Index - 1) + InterpFrac * (MuCacl2_C25(Index) - MuCacl2_C25(Index - 1));
                    Kactual = ConductivityCacl2_C25(Index - 1) + InterpFrac * (ConductivityCacl2_C25(Index) - ConductivityCacl2_C25(Index - 1));
                    PRactual = PrCacl2_C25(Index - 1) + InterpFrac * (PrCacl2_C25(Index) - PrCacl2_C25(Index - 1));
                    Cpactual = CpCacl2_C25(Index - 1) + InterpFrac * (CpCacl2_C25(Index) - CpCacl2_C25(Index - 1));
                }
            } else if (Concentration == 26.00) {
                if (Index == 1) {
                    MUactual = MuCacl2_C26(Index);
                    Kactual = ConductivityCacl2_C26(Index);
                    PRactual = PrCacl2_C26(Index);
                    Cpactual = CpCacl2_C26(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuCacl2_C26(Index);
                    Kactual = ConductivityCacl2_C26(Index);
                    PRactual = PrCacl2_C26(Index);
                    Cpactual = CpCacl2_C26(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuCacl2_C26(Index - 1) + InterpFrac * (MuCacl2_C26(Index) - MuCacl2_C26(Index - 1));
                    Kactual = ConductivityCacl2_C26(Index - 1) + InterpFrac * (ConductivityCacl2_C26(Index) - ConductivityCacl2_C26(Index - 1));
                    PRactual = PrCacl2_C26(Index - 1) + InterpFrac * (PrCacl2_C26(Index) - PrCacl2_C26(Index - 1));
                    Cpactual = CpCacl2_C26(Index - 1) + InterpFrac * (CpCacl2_C26(Index) - CpCacl2_C26(Index - 1));
                }

            } else if (Concentration == 27.00) {
                if (Index == 1) {
                    MUactual = MuCacl2_C27(Index);
                    Kactual = ConductivityCacl2_C27(Index);
                    PRactual = PrCacl2_C27(Index);
                    Cpactual = CpCacl2_C27(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuCacl2_C27(Index);
                    Kactual = ConductivityCacl2_C27(Index);
                    PRactual = PrCacl2_C27(Index);
                    Cpactual = CpCacl2_C27(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuCacl2_C27(Index - 1) + InterpFrac * (MuCacl2_C27(Index) - MuCacl2_C27(Index - 1));
                    Kactual = ConductivityCacl2_C27(Index - 1) + InterpFrac * (ConductivityCacl2_C27(Index) - ConductivityCacl2_C27(Index - 1));
                    PRactual = PrCacl2_C27(Index - 1) + InterpFrac * (PrCacl2_C27(Index) - PrCacl2_C27(Index - 1));
                    Cpactual = CpCacl2_C27(Index - 1) + InterpFrac * (CpCacl2_C27(Index) - CpCacl2_C27(Index - 1));
                }
            } else if (Concentration == 28.00) {
                if (Index == 1) {
                    MUactual = MuCacl2_C28(Index);
                    Kactual = ConductivityCacl2_C28(Index);
                    PRactual = PrCacl2_C28(Index);
                    Cpactual = CpCacl2_C28(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuCacl2_C28(Index);
                    Kactual = ConductivityCacl2_C28(Index);
                    PRactual = PrCacl2_C28(Index);
                    Cpactual = CpCacl2_C28(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuCacl2_C28(Index - 1) + InterpFrac * (MuCacl2_C28(Index) - MuCacl2_C28(Index - 1));
                    Kactual = ConductivityCacl2_C28(Index - 1) + InterpFrac * (ConductivityCacl2_C28(Index) - ConductivityCacl2_C28(Index - 1));
                    PRactual = PrCacl2_C28(Index - 1) + InterpFrac * (PrCacl2_C28(Index) - PrCacl2_C28(Index - 1));
                    Cpactual = CpCacl2_C28(Index - 1) + InterpFrac * (CpCacl2_C28(Index) - CpCacl2_C28(Index - 1));
                }
            } else if (Concentration == 29.00) {
                if (Index == 1) {
                    MUactual = MuCacl2_C29(Index);
                    Kactual = ConductivityCacl2_C29(Index);
                    PRactual = PrCacl2_C29(Index);
                    Cpactual = CpCacl2_C29(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuCacl2_C29(Index);
                    Kactual = ConductivityCacl2_C29(Index);
                    PRactual = PrCacl2_C29(Index);
                    Cpactual = CpCacl2_C29(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuCacl2_C29(Index - 1) + InterpFrac * (MuCacl2_C29(Index) - MuCacl2_C29(Index - 1));
                    Kactual = ConductivityCacl2_C29(Index - 1) + InterpFrac * (ConductivityCacl2_C29(Index) - ConductivityCacl2_C29(Index - 1));
                    PRactual = PrCacl2_C29(Index - 1) + InterpFrac * (PrCacl2_C29(Index) - PrCacl2_C29(Index - 1));
                    Cpactual = CpCacl2_C29(Index - 1) + InterpFrac * (CpCacl2_C29(Index) - CpCacl2_C29(Index - 1));
                }
            } else {
                if (Index == 1) {
                    MUactual = MuCacl2_C30(Index);
                    Kactual = ConductivityCacl2_C30(Index);
                    PRactual = PrCacl2_C30(Index);
                    Cpactual = CpCacl2_C30(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuCacl2_C30(Index);
                    Kactual = ConductivityCacl2_C30(Index);
                    PRactual = PrCacl2_C30(Index);
                    Cpactual = CpCacl2_C30(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuCacl2_C30(Index - 1) + InterpFrac * (MuCacl2_C30(Index) - MuCacl2_C30(Index - 1));
                    Kactual = ConductivityCacl2_C30(Index - 1) + InterpFrac * (ConductivityCacl2_C30(Index) - ConductivityCacl2_C30(Index - 1));
                    PRactual = PrCacl2_C30(Index - 1) + InterpFrac * (PrCacl2_C30(Index) - PrCacl2_C30(Index - 1));
                    Cpactual = CpCacl2_C30(Index - 1) + InterpFrac * (CpCacl2_C30(Index) - CpCacl2_C30(Index - 1));
                }
            }

            // Claculate the Reynold's number
            ReD = 4.0 * RefrigMassFlow / (Pi * MUactual * TubeDiameter);
            if (ReD >= MaxLaminarRe) { // Turbulent flow --> use Colburn equation
                NuD = 0.023 * std::pow(ReD, 0.8) * std::pow(PRactual, 1.0 / 3.0);
            } else { // Laminar flow --> use constant surface temperature relation
                NuD = 3.66;
            }
            NTU = Pi * Kactual * NuD * TubeLength / (RefrigMassFlow * Cpactual);
            if (NTU > MaxExpPower) {
                Effectiveness = 1.0;
            } else {
                Effectiveness = 1.0 - std::exp(-NTU);
            }
        } else if (RefrigType == EG) {
            // Initialize thermal properties of Ethylene Glycol Solution
            if (Concentration == 25.00) {
                if (Index == 1) {
                    MUactual = MuEG_C25(Index);
                    Kactual = ConductivityEG_C25(Index);
                    PRactual = PrEG_C25(Index);
                    Cpactual = CpEG_C25(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuEG_C25(Index);
                    Kactual = ConductivityEG_C25(Index);
                    PRactual = PrEG_C25(Index);
                    Cpactual = CpEG_C25(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuEG_C25(Index - 1) + InterpFrac * (MuEG_C25(Index) - MuEG_C25(Index - 1));
                    Kactual = ConductivityEG_C25(Index - 1) + InterpFrac * (ConductivityEG_C25(Index) - ConductivityEG_C25(Index - 1));
                    PRactual = PrEG_C25(Index - 1) + InterpFrac * (PrEG_C25(Index) - PrEG_C25(Index - 1));
                    Cpactual = CpEG_C25(Index - 1) + InterpFrac * (CpEG_C25(Index) - CpEG_C25(Index - 1));
                }
            } else if (Concentration == 26.00) {
                if (Index == 1) {
                    MUactual = MuEG_C26(Index);
                    Kactual = ConductivityEG_C26(Index);
                    PRactual = PrEG_C26(Index);
                    Cpactual = CpEG_C26(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuEG_C26(Index);
                    Kactual = ConductivityEG_C26(Index);
                    PRactual = PrEG_C26(Index);
                    Cpactual = CpEG_C26(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuEG_C26(Index - 1) + InterpFrac * (MuEG_C26(Index) - MuEG_C26(Index - 1));
                    Kactual = ConductivityEG_C26(Index - 1) + InterpFrac * (ConductivityEG_C26(Index) - ConductivityEG_C26(Index - 1));
                    PRactual = PrEG_C26(Index - 1) + InterpFrac * (PrEG_C26(Index) - PrEG_C26(Index - 1));
                    Cpactual = CpEG_C26(Index - 1) + InterpFrac * (CpEG_C26(Index) - CpEG_C26(Index - 1));
                }

            } else if (Concentration == 27.00) {
                if (Index == 1) {
                    MUactual = MuEG_C27(Index);
                    Kactual = ConductivityEG_C27(Index);
                    PRactual = PrEG_C27(Index);
                    Cpactual = CpEG_C27(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuEG_C27(Index);
                    Kactual = ConductivityEG_C27(Index);
                    PRactual = PrEG_C27(Index);
                    Cpactual = CpEG_C27(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuEG_C27(Index - 1) + InterpFrac * (MuEG_C27(Index) - MuEG_C27(Index - 1));
                    Kactual = ConductivityEG_C27(Index - 1) + InterpFrac * (ConductivityEG_C27(Index) - ConductivityEG_C27(Index - 1));
                    PRactual = PrEG_C27(Index - 1) + InterpFrac * (PrEG_C27(Index) - PrEG_C27(Index - 1));
                    Cpactual = CpEG_C27(Index - 1) + InterpFrac * (CpEG_C27(Index) - CpEG_C27(Index - 1));
                }
            } else if (Concentration == 28.00) {
                if (Index == 1) {
                    MUactual = MuEG_C28(Index);
                    Kactual = ConductivityEG_C28(Index);
                    PRactual = PrEG_C28(Index);
                    Cpactual = CpEG_C28(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuEG_C28(Index);
                    Kactual = ConductivityEG_C28(Index);
                    PRactual = PrEG_C28(Index);
                    Cpactual = CpEG_C28(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuEG_C28(Index - 1) + InterpFrac * (MuEG_C28(Index) - MuEG_C28(Index - 1));
                    Kactual = ConductivityEG_C28(Index - 1) + InterpFrac * (ConductivityEG_C28(Index) - ConductivityEG_C28(Index - 1));
                    PRactual = PrEG_C28(Index - 1) + InterpFrac * (PrEG_C28(Index) - PrEG_C28(Index - 1));
                    Cpactual = CpEG_C28(Index - 1) + InterpFrac * (CpEG_C28(Index) - CpEG_C28(Index - 1));
                }
            } else if (Concentration == 29.00) {
                if (Index == 1) {
                    MUactual = MuEG_C29(Index);
                    Kactual = ConductivityEG_C29(Index);
                    PRactual = PrEG_C29(Index);
                    Cpactual = CpEG_C29(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuEG_C29(Index);
                    Kactual = ConductivityEG_C29(Index);
                    PRactual = PrEG_C29(Index);
                    Cpactual = CpEG_C29(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuEG_C29(Index - 1) + InterpFrac * (MuEG_C29(Index) - MuEG_C29(Index - 1));
                    Kactual = ConductivityEG_C29(Index - 1) + InterpFrac * (ConductivityEG_C29(Index) - ConductivityEG_C29(Index - 1));
                    PRactual = PrEG_C29(Index - 1) + InterpFrac * (PrEG_C29(Index) - PrEG_C29(Index - 1));
                    Cpactual = CpEG_C29(Index - 1) + InterpFrac * (CpEG_C29(Index) - CpEG_C29(Index - 1));
                }
            } else {
                if (Index == 1) {
                    MUactual = MuEG_C30(Index);
                    Kactual = ConductivityEG_C30(Index);
                    PRactual = PrEG_C30(Index);
                    Cpactual = CpEG_C30(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuEG_C30(Index);
                    Kactual = ConductivityEG_C30(Index);
                    PRactual = PrEG_C30(Index);
                    Cpactual = CpEG_C30(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuEG_C30(Index - 1) + InterpFrac * (MuEG_C30(Index) - MuEG_C30(Index - 1));
                    Kactual = ConductivityEG_C30(Index - 1) + InterpFrac * (ConductivityEG_C30(Index) - ConductivityEG_C30(Index - 1));
                    PRactual = PrEG_C30(Index - 1) + InterpFrac * (PrEG_C30(Index) - PrEG_C30(Index - 1));
                    Cpactual = CpEG_C30(Index - 1) + InterpFrac * (CpEG_C30(Index) - CpEG_C30(Index - 1));
                }
            }
            // Claculate the Reynold's number
            ReD = 4.0 * RefrigMassFlow / (Pi * MUactual * TubeDiameter);
            if (ReD >= MaxLaminarRe) { // Turbulent flow --> use Colburn equation
                NuD = 0.023 * std::pow(ReD, 0.8) * std::pow(PRactual, 1.0 / 3.0);
            } else { // Laminar flow --> use constant surface temperature relation
                NuD = 3.66;
            }
            NTU = Pi * Kactual * NuD * TubeLength / (RefrigMassFlow * Cpactual);
            if (NTU > MaxExpPower) {
                Effectiveness = 1.0;

            } else {
                Effectiveness = 1.0 - std::exp(-NTU);
            }
        }
        return Effectiveness;
    }

    void CalcIndirectIndoorIceRink(int const SysNum,     // Index number for the indirect refrigeration system
                                   int const MachineNum, // Number of resurfacers working
                                   Real64 &LoadMet)
    {
        // Using/Aliasing
        using DataHeatBalFanSys::CTFTsrcConstPart;
        using DataHeatBalFanSys::RadSysTiHBConstCoef;
        using DataHeatBalFanSys::RadSysTiHBQsrcCoef;
        using DataHeatBalFanSys::RadSysTiHBToutCoef;
        using DataHeatBalFanSys::RadSysToHBConstCoef;
        using DataHeatBalFanSys::RadSysToHBQsrcCoef;
        using DataHeatBalFanSys::RadSysToHBTinCoef;
        using DataLoopNode::Node;
        using DataPlant::PlantLoop;
        using DataSurfaces::HeatTransferModel_CTF;
        using DataSurfaces::SurfaceClass_Floor;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RefrigNodeIn;          // Node number of the refrigerant entering the refrigeration system
        Real64 RefrigMassFlow;     // Mass flow rate of refrigerant to the floor radiant system
        Real64 RefrigSetPointTemp; // Set point temperature of outlet refrigerant (Used in BOTC)
        Real64 IceSetPointTemp;    // Set point temperature of the ice surface (Used in STC)
        Real64 RefrigTempIn;       // Inlet temperature of the refrigerant to the floor radiant system
        Real64 RefrigTempOut;      // Outlet temperature of the refrigerant from the floor radiant system as defined by a schedule
        int SurfNum;               // DO loop counter for the surfaces that comprise ice rink arena
        int SurfFloor;             // Surface which is a floor
        Real64 EffectivenessIRink; // Effectiveness of the heat exchanger
        int ConstrNum;             // Index for construction number in Construct derived type
        int ControlStrategy;       // Control strategy used for the ice rink (BOTC or STC)
        Real64 ReqMassFlow;        // Required refrigerant mass flow based on the control strategy
        Real64 CpRefrig;           // Specific heat of the refrigerant in the floor radiant system
        Real64 TRefigOutCheck;     // Intermediate check for refrigerant outlet temperature
        int ZoneNum;               // Zone pointer for this floor radiant system
        Real64 IceTemp;            // Temperature of the ice surface
        Real64 TSource;            // Temperature of the source
        Real64 QSetPoint;          // heat to be extracted for ice surface to reach setpoint temperature
        Real64 QRadSysSourceMax;   // Maximum heat transfer possible
        Real64 FreezingLoad;       // Freezing load for the rink (J)
        Real64 ResurfacerHeatLoad; // Resurfacing load (J)
        int SystemType;            // Type of system (For this subroutine it is Indirect System)

        Real64 Ca; // Coefficients to relate the inlet refrigerant temperature to the heat source
        Real64 Cb;
        Real64 Cc;
        Real64 Cd;
        Real64 Ce;
        Real64 Cf;
        Real64 Cg;
        Real64 Ch;
        Real64 Ci;
        Real64 Cj;
        Real64 Ck;
        Real64 Cl;
        static std::string const RoutineName("CalcDirectIndoorIceRinkComps");

        SystemType = IndirectSystem;
        ZoneNum = IRink(SysNum).ZonePtr;
        ControlStrategy = IRink(SysNum).ControlStrategy;

        // Setting the inlet node to the floor radiant system
        RefrigNodeIn = IRink(SysNum).RefrigInNode;
        if (RefrigNodeIn == 0) {
            ShowSevereError("Illegal inlet node for the refrigerant in the direct system");
            ShowFatalError("Preceding condition causes termination");
        }

        if (ControlStrategy == BrineOutletTempControl) {

            // Setting the set point temperature of refrigerant outlet
            if (DRink(SysNum).RefrigSetptSchedPtr > 0) {
                RefrigSetPointTemp = GetCurrentScheduleValue(DRink(SysNum).RefrigSetptSchedPtr);
            } else {
                ShowSevereError("Illegal pointer to brine outlet control strategy");
                ShowFatalError("Preceding condition causes termination");
            }
        } else if (ControlStrategy == SurfaceTempControl) {

            // Setting the set point temperature of ice surface
            if (DRink(SysNum).IceSetptSchedPtr > 0) {
                IceSetPointTemp = GetCurrentScheduleValue(DRink(SysNum).IceSetptSchedPtr);
            } else {
                ShowSevereError("Illegal pointer to surface temperature control strategy");
                ShowFatalError("Preceding condition causes termination");
            }
        } else {
            ShowSevereError("Illegal input for control strategy");
            ShowFatalError("Preceding condition causes termination");
        }
        // Setting the set point temperature of refrigerant outlet
        if (IRink(SysNum).RefrigSetptSchedPtr > 0) {
            RefrigSetPointTemp = GetCurrentScheduleValue(IRink(SysNum).RefrigSetptSchedPtr);
        }

        // Setting the set point temperature of ice surface
        if (IRink(SysNum).IceSetptSchedPtr > 0) {
            IceSetPointTemp = GetCurrentScheduleValue(IRink(SysNum).IceSetptSchedPtr);
        }

        for (SurfNum = 1; SurfNum <= IRink(SysNum).NumOfSurfaces; ++SurfNum) {
            if (Surface(IRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                SurfFloor = IRink(SysNum).SurfacePtrArray(SurfNum);
            }
        }

        RefrigMassFlow = Node(RefrigNodeIn).MassFlowRate;
        RefrigTempIn = Node(RefrigNodeIn).Temp;

        if (RefrigMassFlow <= 0) {
            // No flow or below minimum allowed so there is no heat source/sink
            // This is possible with a mismatch between system and plant operation
            // or a slight mismatch between zone and system controls.  This is not
            // necessarily a "problem" so this exception is necessary in the code.

            QRadSysSource(SurfFloor) = 0.0;
            ReqMassFlow = 0.0;
            SetComponentFlowRate(ReqMassFlow,
                                 IRink(SysNum).RefrigInNode,
                                 IRink(SysNum).RefrigOutNode,
                                 IRink(SysNum).RefrigLoopNum,
                                 IRink(SysNum).RefrigLoopSide,
                                 IRink(SysNum).RefrigBranchNum,
                                 IRink(SysNum).RefrigCompNum);
        } else { // Refrigerant mass flow rate is significant

            // Determine the heat exchanger "effectiveness"
            EffectivenessIRink = CalcEffectivenessIRink(SysNum,
                                                        RefrigTempIn,
                                                        RefrigMassFlow,
                                                        IRink(SysNum).NumCircuits,
                                                        IRink(SysNum).TubeLength,
                                                        IRink(SysNum).TubeDiameter,
                                                        IRink(SysNum).RefrigType,
                                                        IRink(SysNum).RefrigConc);

            ConstrNum = Surface(SurfFloor).Construction;
            CpRefrig = GetSpecificHeatGlycol(
                PlantLoop(IRink(SysNum).RefrigLoopNum).FluidName, RefrigTempIn, PlantLoop(IRink(SysNum).RefrigLoopNum).FluidIndex, RoutineName);
            if (Surface(SurfFloor).HeatTransferAlgorithm == HeatTransferModel_CTF) {

                Ca = RadSysTiHBConstCoef(SurfFloor);
                Cb = RadSysTiHBToutCoef(SurfFloor);
                Cc = RadSysTiHBQsrcCoef(SurfFloor);

                Cd = RadSysToHBConstCoef(SurfFloor);
                Ce = RadSysToHBTinCoef(SurfFloor);
                Cf = RadSysToHBQsrcCoef(SurfFloor);

                Cg = CTFTsrcConstPart(SurfFloor);
                Ch = Construct(ConstrNum).CTFTSourceQ(0);
                Ci = Construct(ConstrNum).CTFTSourceIn(0);
                Cj = Construct(ConstrNum).CTFTSourceOut(0);

                Ck = Cg + ((Ci * (Ca + Cb * Cd) + Cj * (Cd + Ce * Ca)) / (1.0 - Ce * Cb));
                Cl = Ch + ((Ci * (Cc + Cb * Cf) + Cj * (Cf + Ce * Cc)) / (1.0 - Ce * Cb));

                QRadSysSource(SurfFloor) =
                    (RefrigTempIn - Ck) / ((Cl / Surface(SurfFloor).Area) + (1 / (EffectivenessIRink * RefrigMassFlow * CpRefrig)));
                TSource = Ck + (Cl * QRadSysSource(SurfFloor));
                IceTemp = (Ca + (Cb * Cd) + (QRadSysSource(SurfFloor) * (Cc + (Cb * Cf)))) / (1 - (Cb * Ce));
                QRadSysSourceMax = RefrigMassFlow * CpRefrig * (RefrigTempIn - TSource);
            }

            ControlStrategy = IRink(SysNum).ControlStrategy;

            if (ControlStrategy == BrineOutletTempControl) {
                // Finding the required mass flow rate so that the outlet temperature
                // becomes equal to the user defined refrigerant outlet temperature
                // for the given refrigerant inlet temperature.

                ReqMassFlow = (((Ck - RefrigTempIn) / (RefrigSetPointTemp - RefrigTempIn)) - (1 / EffectivenessIRink)) *
                              (Surface(SurfFloor).Area / (CpRefrig * Cl));
                TRefigOutCheck = RefrigTempIn - ((QRadSysSource(SurfFloor)) / RefrigMassFlow * CpRefrig);

                if (TRefigOutCheck <= (RefrigSetPointTemp)) {
                    RefrigMassFlow = IRink(SysNum).MinRefrigMassFlow;
                    SetComponentFlowRate(RefrigMassFlow,
                                         IRink(SysNum).RefrigInNode,
                                         IRink(SysNum).RefrigOutNode,
                                         IRink(SysNum).RefrigLoopNum,
                                         IRink(SysNum).RefrigLoopSide,
                                         IRink(SysNum).RefrigBranchNum,
                                         IRink(SysNum).RefrigCompNum);
                } else {
                    if (ReqMassFlow >= IRink(SysNum).MaxRefrigMassFlow) {
                        RefrigMassFlow = IRink(SysNum).MaxRefrigMassFlow;
                        SetComponentFlowRate(RefrigMassFlow,
                                             IRink(SysNum).RefrigInNode,
                                             IRink(SysNum).RefrigOutNode,
                                             IRink(SysNum).RefrigLoopNum,
                                             IRink(SysNum).RefrigLoopSide,
                                             IRink(SysNum).RefrigBranchNum,
                                             IRink(SysNum).RefrigCompNum);
                    } else {
                        RefrigMassFlow = ReqMassFlow;
                        SetComponentFlowRate(RefrigMassFlow,
                                             IRink(SysNum).RefrigInNode,
                                             IRink(SysNum).RefrigOutNode,
                                             IRink(SysNum).RefrigLoopNum,
                                             IRink(SysNum).RefrigLoopSide,
                                             IRink(SysNum).RefrigBranchNum,
                                             IRink(SysNum).RefrigCompNum);
                    }
                }
            } else if (ControlStrategy == SurfaceTempControl) {
                // Finding heat thant needs to be extraced so that the
                // ice surface reaches the set point temperature.
                QSetPoint = ((((1 - (Cb * Ce)) * IceSetPointTemp) - Ca - (Cb * Cd)) / (Cc + (Cb * Cf))) * Surface(SurfFloor).Area;
                ReqMassFlow = QSetPoint / (EffectivenessIRink * CpRefrig);
                if (IceTemp <= IceSetPointTemp) {
                    QRadSysSource(SurfFloor) = 0.0;
                    RefrigMassFlow = 0.0;
                    SetComponentFlowRate(RefrigMassFlow,
                                         IRink(SysNum).RefrigInNode,
                                         IRink(SysNum).RefrigOutNode,
                                         IRink(SysNum).RefrigLoopNum,
                                         IRink(SysNum).RefrigLoopSide,
                                         IRink(SysNum).RefrigBranchNum,
                                         IRink(SysNum).RefrigCompNum);
                } else {
                    if (QRadSysSourceMax <= QSetPoint) {
                        RefrigMassFlow = IRink(SysNum).MaxRefrigMassFlow;
                        SetComponentFlowRate(RefrigMassFlow,
                                             IRink(SysNum).RefrigInNode,
                                             IRink(SysNum).RefrigOutNode,
                                             IRink(SysNum).RefrigLoopNum,
                                             IRink(SysNum).RefrigLoopSide,
                                             IRink(SysNum).RefrigBranchNum,
                                             IRink(SysNum).RefrigCompNum);
                    } else {
                        RefrigMassFlow = ReqMassFlow;
                        SetComponentFlowRate(RefrigMassFlow,
                                             IRink(SysNum).RefrigInNode,
                                             IRink(SysNum).RefrigOutNode,
                                             IRink(SysNum).RefrigLoopNum,
                                             IRink(SysNum).RefrigLoopSide,
                                             IRink(SysNum).RefrigBranchNum,
                                             IRink(SysNum).RefrigCompNum);
                    }
                }
            }
            IRink(SysNum).RefrigMassFlow = RefrigMassFlow;

            // "Temperature Comparision" cut-off
            // Check if QRadSysSource is positive i.e. it is actually heating the rink.
            // If so then the refrigeration system is doing opposite of its intension
            // and it needs to be shut down.

            if (QRadSysSource(SurfFloor) >= 0.0) {
                RefrigMassFlow = 0.0;
                SetComponentFlowRate(RefrigMassFlow,
                                     IRink(SysNum).RefrigInNode,
                                     IRink(SysNum).RefrigOutNode,
                                     IRink(SysNum).RefrigLoopNum,
                                     IRink(SysNum).RefrigLoopSide,
                                     IRink(SysNum).RefrigBranchNum,
                                     IRink(SysNum).RefrigCompNum);
                IRink(SysNum).RefrigMassFlow = RefrigMassFlow;
            }
        }
        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(ZoneNum);
        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(ZoneNum);

        IceRinkFreezing(FreezingLoad, SysNum, SystemType);
        IceRinkResurfacer(ResurfacerHeatLoad, SysNum, SystemType, MachineNum);
        LoadMet = FreezingLoad + ResurfacerHeatLoad;
    }

    void CalcDirectIndoorIceRink(int const SysNum,     // Index number for the indirect refrigeration system
                                 int const MachineNum, // Number of resurfacers working
                                 Real64 &LoadMet)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Punya Sloka Dash
        //       DATE WRITTEN   August 2019

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves the direct type refrigeration system based on
        // how much refrigerant is (and the conditions of the refrigerant) supplied
        // to the radiant system. (NOTE: The refrigerant in direct system is ammonia, NH3).

        // METHODOLOGY EMPLOYED:
        // Use heat exchanger formulas to obtain the heat source/sink for the radiant
        // system based on the inlet conditions and flow rate of refrigerant. Once that is
        // determined, recalculate the surface heat balances to reflect this heat
        // addition/subtraction.  The load met by the system is determined by the
        // difference between the convection from all surfaces in the zone when
        // there was no radiant system output and with a source/sink added.

        // Using/Aliasing
        using DataHeatBalFanSys::CTFTsrcConstPart;
        using DataHeatBalFanSys::RadSysTiHBConstCoef;
        using DataHeatBalFanSys::RadSysTiHBQsrcCoef;
        using DataHeatBalFanSys::RadSysTiHBToutCoef;
        using DataHeatBalFanSys::RadSysToHBConstCoef;
        using DataHeatBalFanSys::RadSysToHBQsrcCoef;
        using DataHeatBalFanSys::RadSysToHBTinCoef;
        using DataLoopNode::Node;
        using DataPlant::PlantLoop;
        using DataSurfaces::HeatTransferModel_CTF;
        using DataSurfaces::SurfaceClass_Floor;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RefrigNodeIn;          // Node number of the refrigerant entering the refrigeration system
        Real64 RefrigMassFlow;     // Mass flow rate of refrigerant to the floor radiant system
        Real64 RefrigSetPointTemp; // Set point temperature of outlet refrigerant (Used in BOTC)
        Real64 IceSetPointTemp;    // Set point temperature of the ice surface (Used in STC)
        Real64 RefrigTempIn;       // Inlet temperature of the refrigerant to the floor radiant system
        Real64 RefrigTempOut;      // Outlet temperature of the refrigerant from the floor radiant system as defined by a schedule
        int SurfNum;               // DO loop counter for the surfaces that comprise ice rink arena
        int SurfFloor;             // Surface which is a floor
        Real64 EffectivenessDRink; // Effectiveness of the heat exchanger
        int ConstrNum;             // Index for construction number in Construct derived type
        int ControlStrategy;       // Control strategy used for the ice rink (BOTC or STC)
        Real64 ReqMassFlow;        // Required refrigerant mass flow based on the control strategy
        Real64 CpRefrig;           // Specific heat of the refrigerant in the floor radiant system
        Real64 TRefigOutCheck;     // Intermediate check for refrigerant outlet temperature
        int ZoneNum;               // Zone pointer for this floor radiant system
        Real64 IceTemp;            // Temperature of the ice surface
        Real64 TSource;            // Temperature of the source
        Real64 QSetPoint;          // heat to be extracted for ice surface to reach setpoint temperature
        Real64 QRadSysSourceMax;   // Maximum heat transfer possible
        Real64 FreezingLoad;       // Freezing load for the rink (J)
        Real64 ResurfacerHeatLoad; // Resurfacing load (J)
        int SystemType;            // Type of system (For this subroutine it is Direct System)

        Real64 Ca; // Coefficients to relate the inlet refrigerant temperature to the heat source
        Real64 Cb;
        Real64 Cc;
        Real64 Cd;
        Real64 Ce;
        Real64 Cf;
        Real64 Cg;
        Real64 Ch;
        Real64 Ci;
        Real64 Cj;
        Real64 Ck;
        Real64 Cl;
        static std::string const RoutineName("CalcDirectIndoorIceRinkComps");

        SystemType = DirectSystem;
        ZoneNum = DRink(SysNum).ZonePtr;
        ControlStrategy = DRink(SysNum).ControlStrategy;
        // Setting the inlet node to the floor radiant system
        RefrigNodeIn = DRink(SysNum).RefrigInNode;
        if (RefrigNodeIn == 0) {
            ShowSevereError("Illegal inlet node for the refrigerant in the direct system");
            ShowFatalError("Preceding condition causes termination");
        }

        if (ControlStrategy == BrineOutletTempControl) {

            // Setting the set point temperature of refrigerant outlet
            if (DRink(SysNum).RefrigSetptSchedPtr > 0) {
                RefrigSetPointTemp = GetCurrentScheduleValue(DRink(SysNum).RefrigSetptSchedPtr);
            } else {
                ShowSevereError("Illegal pointer to brine outlet control strategy");
                ShowFatalError("Preceding condition causes termination");
            }
        } else if (ControlStrategy == SurfaceTempControl) {

            // Setting the set point temperature of ice surface
            if (DRink(SysNum).IceSetptSchedPtr > 0) {
                IceSetPointTemp = GetCurrentScheduleValue(DRink(SysNum).IceSetptSchedPtr);
            } else {
                ShowSevereError("Illegal pointer to surface temperature control strategy");
                ShowFatalError("Preceding condition causes termination");
            }
        } else {
            ShowSevereError("Illegal input for control strategy");
            ShowFatalError("Preceding condition causes termination");
        }

        for (SurfNum = 1; SurfNum <= DRink(SysNum).NumOfSurfaces; ++SurfNum) {
            if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                SurfFloor = DRink(SysNum).SurfacePtrArray(SurfNum);
            }
        }

        RefrigMassFlow = Node(RefrigNodeIn).MassFlowRate;
        RefrigTempIn = Node(RefrigNodeIn).Temp;

        if (RefrigMassFlow <= 0) {
            // No flow or below minimum allowed so there is no heat source/sink
            // This is possible with a mismatch between system and plant operation
            // or a slight mismatch between zone and system controls.  This is not
            // necessarily a "problem" so this exception is necessary in the code.

            QRadSysSource(SurfFloor) = 0.0;
            ReqMassFlow = 0.0;
            SetComponentFlowRate(ReqMassFlow,
                                 DRink(SysNum).RefrigInNode,
                                 DRink(SysNum).RefrigOutNode,
                                 DRink(SysNum).RefrigLoopNum,
                                 DRink(SysNum).RefrigLoopSide,
                                 DRink(SysNum).RefrigBranchNum,
                                 DRink(SysNum).RefrigCompNum);
        } else { // Refrigerant mass flow rate is significant

            // Determine the heat exchanger "effectiveness"
            EffectivenessDRink = CalcEffectivenessDRink(
                SysNum, RefrigTempIn, RefrigMassFlow, DRink(SysNum).NumCircuits, DRink(SysNum).TubeLength, DRink(SysNum).TubeDiameter);
            ConstrNum = Surface(SurfFloor).Construction;
            CpRefrig = GetSpecificHeatGlycol(
                PlantLoop(DRink(SysNum).RefrigLoopNum).FluidName, RefrigTempIn, PlantLoop(DRink(SysNum).RefrigLoopNum).FluidIndex, RoutineName);
            if (Surface(SurfFloor).HeatTransferAlgorithm == HeatTransferModel_CTF) {

                Ca = RadSysTiHBConstCoef(SurfFloor);
                Cb = RadSysTiHBToutCoef(SurfFloor);
                Cc = RadSysTiHBQsrcCoef(SurfFloor);

                Cd = RadSysToHBConstCoef(SurfFloor);
                Ce = RadSysToHBTinCoef(SurfFloor);
                Cf = RadSysToHBQsrcCoef(SurfFloor);

                Cg = CTFTsrcConstPart(SurfFloor);
                Ch = Construct(ConstrNum).CTFTSourceQ(0);
                Ci = Construct(ConstrNum).CTFTSourceIn(0);
                Cj = Construct(ConstrNum).CTFTSourceOut(0);

                Ck = Cg + ((Ci * (Ca + Cb * Cd) + Cj * (Cd + Ce * Ca)) / (1.0 - Ce * Cb));
                Cl = Ch + ((Ci * (Cc + Cb * Cf) + Cj * (Cf + Ce * Cc)) / (1.0 - Ce * Cb));

                QRadSysSource(SurfFloor) =
                    (RefrigTempIn - Ck) / ((Cl / Surface(SurfFloor).Area) + (1 / (EffectivenessDRink * RefrigMassFlow * CpRefrig)));
                TSource = Ck + (Cl * QRadSysSource(SurfFloor));
                IceTemp = (Ca + (Cb * Cd) + (QRadSysSource(SurfFloor) * (Cc + (Cb * Cf)))) / (1 - (Cb * Ce));
                QRadSysSourceMax = RefrigMassFlow * CpRefrig * (RefrigTempIn - TSource);
            }

            if (ControlStrategy == BrineOutletTempControl) {
                // Finding the required mass flow rate so that the outlet temperature
                // becomes equal to the user defined refrigerant outlet temperature
                // for the given refrigerant inlet temperature.

                ReqMassFlow = (((Ck - RefrigTempIn) / (RefrigSetPointTemp - RefrigTempIn)) - (1 / EffectivenessDRink)) *
                              (Surface(SurfFloor).Area / (CpRefrig * Cl));
                TRefigOutCheck = RefrigTempIn - ((QRadSysSource(SurfFloor)) / RefrigMassFlow * CpRefrig);

                if (TRefigOutCheck <= (RefrigSetPointTemp)) {
                    RefrigMassFlow = DRink(SysNum).MinRefrigMassFlow;
                    SetComponentFlowRate(RefrigMassFlow,
                                         DRink(SysNum).RefrigInNode,
                                         DRink(SysNum).RefrigOutNode,
                                         DRink(SysNum).RefrigLoopNum,
                                         DRink(SysNum).RefrigLoopSide,
                                         DRink(SysNum).RefrigBranchNum,
                                         DRink(SysNum).RefrigCompNum);
                } else {
                    if (ReqMassFlow >= DRink(SysNum).MaxRefrigMassFlow) {
                        RefrigMassFlow = DRink(SysNum).MaxRefrigMassFlow;
                        SetComponentFlowRate(RefrigMassFlow,
                                             DRink(SysNum).RefrigInNode,
                                             DRink(SysNum).RefrigOutNode,
                                             DRink(SysNum).RefrigLoopNum,
                                             DRink(SysNum).RefrigLoopSide,
                                             DRink(SysNum).RefrigBranchNum,
                                             DRink(SysNum).RefrigCompNum);
                    } else {
                        RefrigMassFlow = ReqMassFlow;
                        SetComponentFlowRate(RefrigMassFlow,
                                             DRink(SysNum).RefrigInNode,
                                             DRink(SysNum).RefrigOutNode,
                                             DRink(SysNum).RefrigLoopNum,
                                             DRink(SysNum).RefrigLoopSide,
                                             DRink(SysNum).RefrigBranchNum,
                                             DRink(SysNum).RefrigCompNum);
                    }
                }
            } else if (ControlStrategy == SurfaceTempControl) {
                // Finding heat thant needs to be extraced so that the
                // ice surface reaches the set point temperature.
                QSetPoint = ((((1 - (Cb * Ce)) * IceSetPointTemp) - Ca - (Cb * Cd)) / (Cc + (Cb * Cf))) * Surface(SurfFloor).Area;
                ReqMassFlow = QSetPoint / (EffectivenessDRink * CpRefrig);
                if (IceTemp <= IceSetPointTemp) {
                    QRadSysSource(SurfFloor) = 0.0;
                    RefrigMassFlow = 0.0;
                    SetComponentFlowRate(RefrigMassFlow,
                                         DRink(SysNum).RefrigInNode,
                                         DRink(SysNum).RefrigOutNode,
                                         DRink(SysNum).RefrigLoopNum,
                                         DRink(SysNum).RefrigLoopSide,
                                         DRink(SysNum).RefrigBranchNum,
                                         DRink(SysNum).RefrigCompNum);
                } else {
                    if (QRadSysSourceMax <= QSetPoint) {
                        RefrigMassFlow = DRink(SysNum).MaxRefrigMassFlow;
                        SetComponentFlowRate(RefrigMassFlow,
                                             DRink(SysNum).RefrigInNode,
                                             DRink(SysNum).RefrigOutNode,
                                             DRink(SysNum).RefrigLoopNum,
                                             DRink(SysNum).RefrigLoopSide,
                                             DRink(SysNum).RefrigBranchNum,
                                             DRink(SysNum).RefrigCompNum);
                    } else {
                        RefrigMassFlow = ReqMassFlow;
                        SetComponentFlowRate(RefrigMassFlow,
                                             DRink(SysNum).RefrigInNode,
                                             DRink(SysNum).RefrigOutNode,
                                             DRink(SysNum).RefrigLoopNum,
                                             DRink(SysNum).RefrigLoopSide,
                                             DRink(SysNum).RefrigBranchNum,
                                             DRink(SysNum).RefrigCompNum);
                    }
                }
            }
            DRink(SysNum).RefrigMassFlow = RefrigMassFlow;

            // "Temperature Comparision" cut-off
            // Check if QRadSysSource is positive i.e. it is actually heating the rink.
            // If so then the refrigeration system is doing opposite of its intension
            // and it needs to be shut down.

            if (QRadSysSource(SurfFloor) >= 0.0) {
                RefrigMassFlow = 0.0;
                SetComponentFlowRate(RefrigMassFlow,
                                     DRink(SysNum).RefrigInNode,
                                     DRink(SysNum).RefrigOutNode,
                                     DRink(SysNum).RefrigLoopNum,
                                     DRink(SysNum).RefrigLoopSide,
                                     DRink(SysNum).RefrigBranchNum,
                                     DRink(SysNum).RefrigCompNum);
                DRink(SysNum).RefrigMassFlow = RefrigMassFlow;
            }
        }
        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(ZoneNum);
        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(ZoneNum);

        IceRinkFreezing(FreezingLoad, SysNum, SystemType);
        IceRinkResurfacer(ResurfacerHeatLoad, SysNum, SystemType, MachineNum);
        LoadMet = FreezingLoad + ResurfacerHeatLoad;
    }

    void UpdateIndoorIceRink(bool const EP_UNUSED(FirstHVACIteration), // TRUE if 1st HVAC simulation of system timestep
                             int const SysNum,                         // Index to the floor radiant system
                             int const SystemType)                     // Type of system (Direct or Indirect)
    {
        // Using/Aliasing
        using DataGlobals::TimeStepZone;
        using DataHeatBalance::Zone;
        using DataHVACGlobals::SysTimeElapsed;
        using DataHVACGlobals::TimeStepSys;
        using DataLoopNode::Node;
        using DataPlant::PlantLoop;
        using DataSurfaces::SurfaceClass_Floor;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::SafeCopyPlantNode;
        using PlantUtilities::SetComponentFlowRate;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("UpdateIndoorIceRink");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // int Rink;                // Number of Direct type indoor ice rinks
        int SurfNum;             // LOOP counter for surface
        int SurfFloor;           // Surface which is a floor
        Real64 TotalRadSysPower; // Total source/sink power for the radiant system (sum of all floors of direct type rinks)
        int ZoneNum;             // Zone for this system
        int RefrigInletNode;     // Refrigerant inlet node
        int RefrigOutletNode;    // Refrigerant outlet node
        Real64 RefrigMassFlow;   // Refrigerant mass flow rate
        Real64 CpRefrig;         // Specific heat of refrigerant
        int TotNumSurfaces;      // Total number of surfaces in an ice rink arena
        Real64 ZoneMult;         // Zone multiplier

        {
            auto const SELECT_CASE_var(SystemType);
            if (SELECT_CASE_var == DirectSystem) {
                TotNumSurfaces = DRink(SysNum).NumOfSurfaces;
            } else if (SELECT_CASE_var == IndirectSystem) {
                TotNumSurfaces = IRink(SysNum).NumOfSurfaces;
            } else {
                assert(false);
            }

            for (SurfNum = 1; SurfNum <= TotNumSurfaces; ++SurfNum) {
                {
                    auto const SELECT_CASE_var(SystemType);
                    if (SELECT_CASE_var == DirectSystem) {
                        if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                            SurfFloor = DRink(SysNum).SurfacePtrArray(SurfNum);
                        }
                    } else if (SELECT_CASE_var == IndirectSystem) {
                        if (Surface(IRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                            SurfFloor = IRink(SysNum).SurfacePtrArray(SurfNum);
                        }
                    } else {
                        assert(false);
                    }
                }
            }
        }

        if (LastSysTimeElapsed(SurfFloor) == SysTimeElapsed) {
            // Still iterating or reducing system time step, so subtract old values which were
            // not valid
            QRadSysSrcAvg(SurfFloor) -= LastQRadSysSrc(SurfFloor) * LastTimeStepSys(SurfFloor) / TimeStepZone;
        }

        // Update the running average and the "last" values with the current values of the appropriate variables
        QRadSysSrcAvg(SurfFloor) += QRadSysSource(SurfFloor) * TimeStepSys / TimeStepZone;

        LastQRadSysSrc(SurfFloor) = QRadSysSource(SurfFloor);
        LastSysTimeElapsed(SurfFloor) = SysTimeElapsed;
        LastTimeStepSys(SurfFloor) = TimeStepSys;

        // For the direct type system, calculate the refrigerant side outlet conditions
        if (SystemType == DirectSystem) {
            TotalRadSysPower = 0.0;
            for (SurfNum = 1; SurfNum <= TotNumSurfaces; ++SurfNum) {
                if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                    SurfFloor = DRink(SysNum).SurfacePtrArray(SurfNum);
                    TotalRadSysPower = QRadSysSource(SurfFloor);
                }
            }
            ZoneNum = DRink(SysNum).ZonePtr;
            ZoneMult = double(Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier);
            TotalRadSysPower *= ZoneMult;

            RefrigInletNode = DRink(SysNum).RefrigInNode;
            RefrigOutletNode = DRink(SysNum).RefrigOutNode;
            RefrigMassFlow = Node(RefrigInletNode).MassFlowRate;
            CpRefrig = GetSpecificHeatGlycol(PlantLoop(DRink(SysNum).RefrigLoopNum).FluidName,
                                             Node(RefrigInletNode).Temp,
                                             PlantLoop(DRink(SysNum).RefrigLoopNum).FluidIndex,
                                             RoutineName);
            if (OperatingMode == CoolingMode) {

                if ((CpRefrig > 0.0) && (RefrigMassFlow > 0.0)) {
                    SafeCopyPlantNode(RefrigInletNode, RefrigOutletNode);
                    Node(RefrigOutletNode).Temp = Node(RefrigInletNode).Temp - TotalRadSysPower / RefrigMassFlow / CpRefrig;
                }
            } else { // Not operating
                SafeCopyPlantNode(RefrigInletNode, RefrigOutletNode);
            }
            CheckForOutOfRangeTempResult(SystemType, SysNum, Node(RefrigOutletNode).Temp, Node(RefrigInletNode).Temp, RefrigMassFlow);
        }

        if (SystemType == IndirectSystem) {
            TotalRadSysPower = 0.0;
            for (SurfNum = 1; SurfNum <= TotNumSurfaces; ++SurfNum) {
                if (Surface(IRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                    SurfFloor = IRink(SysNum).SurfacePtrArray(SurfNum);
                    TotalRadSysPower = QRadSysSource(SurfFloor);
                }
            }
            ZoneNum = IRink(SysNum).ZonePtr;
            ZoneMult = double(Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier);
            TotalRadSysPower *= ZoneMult;

            RefrigInletNode = IRink(SysNum).RefrigInNode;
            RefrigOutletNode = IRink(SysNum).RefrigOutNode;
            RefrigMassFlow = Node(RefrigInletNode).MassFlowRate;
            CpRefrig = GetSpecificHeatGlycol(PlantLoop(IRink(SysNum).RefrigLoopNum).FluidName,
                                             Node(RefrigInletNode).Temp,
                                             PlantLoop(IRink(SysNum).RefrigLoopNum).FluidIndex,
                                             RoutineName);
            if (OperatingMode == CoolingMode) {

                if ((CpRefrig > 0.0) && (RefrigMassFlow > 0.0)) {
                    SafeCopyPlantNode(RefrigInletNode, RefrigOutletNode);
                    Node(RefrigOutletNode).Temp = Node(RefrigInletNode).Temp - TotalRadSysPower / RefrigMassFlow / CpRefrig;
                }
            } else { // Not operating
                SafeCopyPlantNode(RefrigInletNode, RefrigOutletNode);
            }
            CheckForOutOfRangeTempResult(SystemType, SysNum, Node(RefrigOutletNode).Temp, Node(RefrigInletNode).Temp, RefrigMassFlow);
        }
    }

    void UpdateRinkRadSysSourceValAvg(bool &RefrigSysOn) // .TRUE. if the refrigeration system has run this zone time step
    {
        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CloseEnough(0.01); // Some arbitrarily small value to avoid zeros and numbers that are almost the same

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // DO loop counter for surface index

        RefrigSysOn = false;

        // If this was never allocated, then there are no radiant systems in this input file (just RETURN)
        if (!allocated(QRadSysSrcAvg)) return;

        // If it was allocated, then we have to check to see if this was running at all...
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (QRadSysSrcAvg(SurfNum) != 0.0) {
                RefrigSysOn = true;
                break; // DO loop
            }
        }

        QRadSysSource = QRadSysSrcAvg;

        // For interzone surfaces, QRadSysSrcAvg was only updated for the "active" side.  The active side
        // would have a non-zero value at this point.  If the numbers differ, then we have to manually update.
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum) {
                if (std::abs(QRadSysSource(SurfNum) - QRadSysSource(Surface(SurfNum).ExtBoundCond)) > CloseEnough) { // numbers differ
                    if (std::abs(QRadSysSource(SurfNum)) > std::abs(QRadSysSource(Surface(SurfNum).ExtBoundCond))) {
                        QRadSysSource(Surface(SurfNum).ExtBoundCond) = QRadSysSource(SurfNum);
                    } else {
                        QRadSysSource(SurfNum) = QRadSysSource(Surface(SurfNum).ExtBoundCond);
                    }
                }
            }
        }
    }

    void CheckForOutOfRangeTempResult(
        int const SystemType, int const SysNum, Real64 const outletTemp, Real64 const inletTemp, Real64 const EP_UNUSED(mdot))
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   March 2013

        // PURPOSE OF THIS SUBROUTINE:
        // check for crazy, out of range temperature results for fluid leaving radiant system

        // Using/Aliasing
        using General::RoundSigDigits;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const UpperRangeLimit(500.0);  // high error trigger limit for when model is not working
        Real64 const LowerRangeLimit(-300.0); // Low error trigger limit for when model is not working

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool WarnTooLow(false);
        static bool WarnTooHigh(false);

        WarnTooLow = false;
        WarnTooHigh = false;
        if (outletTemp < LowerRangeLimit) {
            WarnTooLow = true;
        }

        if (outletTemp > UpperRangeLimit) {
            WarnTooHigh = true;
        }

        if (WarnTooLow || WarnTooHigh) {

            {
                auto const SELECT_CASE_var(SystemType);
                if (SELECT_CASE_var == DirectSystem) {
                    if (WarnTooLow) {
                        if (DRink(SysNum).OutRangeLoErrorCount == 0) {
                            ShowSevereMessage("UpdateIndoorIceRink: model result for fluid outlet temperature is not physical.");
                            ShowContinueError("Occurs for floor radiant system name = " + DRink(SysNum).Name);
                            ShowContinueError("Calculated floor radiant system outlet temperature = " + RoundSigDigits(outletTemp, 3) + " [C]");
                            ShowContinueError("Floor radiant system inlet temperature = " + RoundSigDigits(inletTemp, 3) + " [C]");
                            ShowContinueError("A possible cause is that the materials used in the internal source construction are not "
                                              "compatible with the model.");
                        }
                        ShowRecurringSevereErrorAtEnd(
                            "UpdateIndoorIceRink: Detected low out of range outlet temperature result for radiant system name =" + DRink(SysNum).Name,
                            DRink(SysNum).OutRangeLoErrorCount,
                            outletTemp,
                            outletTemp);
                    }
                    if (WarnTooHigh) {
                        if (DRink(SysNum).OutRangeHiErrorCount == 0) {
                            ShowSevereMessage("UpdateIndoorIceRink: model result for fluid outlet temperature is not physical.");
                            ShowContinueError("Occurs for floor radiant system name = " + DRink(SysNum).Name);
                            ShowContinueError("Calculated floor radiant system outlet temperature = " + RoundSigDigits(outletTemp, 3) + " [C]");
                            ShowContinueError("Floor radiant system inlet temperature = " + RoundSigDigits(inletTemp, 3) + " [C]");
                            ShowContinueError("A possible cause is that the materials used in the internal source construction are not "
                                              "compatible with the model.");
                        }
                        ShowRecurringSevereErrorAtEnd(
                            "UpdateLowTempRadiantSystem: Detected high out of range outlet temperature result radiant system name =" +
                                DRink(SysNum).Name,
                            DRink(SysNum).OutRangeHiErrorCount,
                            outletTemp,
                            outletTemp);
                    }

                } else if (SELECT_CASE_var == IndirectSystem) {
                    if (WarnTooLow) {

                        if (IRink(SysNum).OutRangeLoErrorCount == 0) {
                            ShowSevereMessage("UpdateIndoorIceRink: model result for fluid outlet temperature is not physical.");
                            ShowContinueError("Occurs for floor radiant system name = " + IRink(SysNum).Name);
                            ShowContinueError("Calculated floor radiant system outlet temperature = " + RoundSigDigits(outletTemp, 3) + " [C]");
                            ShowContinueError("Floor radiant system inlet temperature = " + RoundSigDigits(inletTemp, 3) + " [C]");
                            ShowContinueError("A possible cause is that the materials used in the internal source construction are not "
                                              "compatible with the model.");
                        }
                        ShowRecurringSevereErrorAtEnd("UpdateIndoorIceRink: Detected high out of range temperature result for radiant system name =" +
                                                          IRink(SysNum).Name,
                                                      IRink(SysNum).OutRangeLoErrorCount,
                                                      outletTemp,
                                                      outletTemp);
                    }
                    if (WarnTooHigh) {
                        if (IRink(SysNum).OutRangeHiErrorCount == 0) {
                            ShowSevereMessage("UpdateLowTempRadiantSystem: model result for fluid outlet temperature is not physical.");
                            ShowContinueError("Occurs for radiant system name = " + IRink(SysNum).Name);
                            ShowContinueError("Calculated radiant system outlet temperature = " + RoundSigDigits(outletTemp, 3) + " [C]");
                            ShowContinueError("Radiant system inlet temperature = " + RoundSigDigits(inletTemp, 3) + " [C]");
                            ShowContinueError("A possible cause is that the materials used in the internal source construction are not "
                                              "compatible with the model.");
                        }
                        ShowRecurringSevereErrorAtEnd("UpdateIndoorIceRink: Detected high out of range temperature result for radiant system name =" +
                                                          IRink(SysNum).Name,
                                                      IRink(SysNum).OutRangeHiErrorCount,
                                                      outletTemp,
                                                      outletTemp);
                    }
                }
            }
        }
    }

    Real64 SumHATsurf(int const ZoneNum) // Zone number
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   July 2003

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
        // The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
        // and should be updated accordingly.

        // Using/Aliasing
        using namespace DataSurfaces;
        using namespace DataHeatBalance;
        using namespace DataHeatBalSurface;

        // Return value
        Real64 SumHATsurf;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // Surface number
        Real64 Area; // Effective surface area

        SumHATsurf = 0.0;

        for (SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
            if (!Surface(SurfNum).HeatTransSurf) continue; // Skip non-heat transfer surfaces

            Area = Surface(SurfNum).Area;

            if (Surface(SurfNum).Class == SurfaceClass_Window) {
                if (SurfaceWindow(SurfNum).ShadingFlag == IntShadeOn || SurfaceWindow(SurfNum).ShadingFlag == IntBlindOn) {
                    // The area is the shade or blind are = sum of the glazing area and the divider area (which is zero if no divider)
                    Area += SurfaceWindow(SurfNum).DividerArea;
                }

                if (SurfaceWindow(SurfNum).FrameArea > 0.0) {
                    // Window frame contribution
                    SumHATsurf += HConvIn(SurfNum) * SurfaceWindow(SurfNum).FrameArea * (1.0 + SurfaceWindow(SurfNum).ProjCorrFrIn) *
                                  SurfaceWindow(SurfNum).FrameTempSurfIn;
                }

                if (SurfaceWindow(SurfNum).DividerArea > 0.0 && SurfaceWindow(SurfNum).ShadingFlag != IntShadeOn &&
                    SurfaceWindow(SurfNum).ShadingFlag != IntBlindOn) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    SumHATsurf += HConvIn(SurfNum) * SurfaceWindow(SurfNum).DividerArea * (1.0 + 2.0 * SurfaceWindow(SurfNum).ProjCorrDivIn) *
                                  SurfaceWindow(SurfNum).DividerTempSurfIn;
                }
            }

            SumHATsurf += HConvIn(SurfNum) * Area * TempSurfInTmp(SurfNum);
        }

        return SumHATsurf;
    }

    void ReportIndoorIceRink(int const SysNum,     // Index to the floor radiant system
                             int const SystemType) // Type of system (Direct or Indirect)
    {
        // Using/Aliasing
        using DataGlobals::SecInHour;
        using DataHeatBalance::Zone;
        using DataHVACGlobals::TimeStepSys;
        using DataLoopNode::Node;
        using DataPlant::PlantLoop;
        using DataSurfaces::Surface;
        using DataSurfaces::SurfaceClass_Floor;
        using FluidProperties::GetSpecificHeatGlycol;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("ReportIndoorIceRink");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Rink;                // Number of Direct type indoor ice rinks
        int SurfNum;             // LOOP counter for surface
        int SurfFloor;           // Surface which is a floor
        Real64 TotalRadSysPower; // Total source/sink power for the radiant system (sum of all floors of direct type rinks)
        {
            auto const SELECT_CASE_var(SystemType);

            if (SELECT_CASE_var == DirectSystem) {
                for (Rink = 1; Rink <= NumOfDirectRefrigSys; ++Rink) {
                    for (SurfNum = 1; SurfNum <= DRink(SysNum).NumOfSurfaces; ++SurfNum) {
                        if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                            SurfFloor = DRink(SysNum).SurfacePtrArray(SurfNum);
                            TotalRadSysPower += QRadSysSource(SurfFloor);
                        }
                    }
                }

                DRink(SysNum).CoolPower = 0.0;

                {
                    auto const SELECT_CASE_var1(OperatingMode);

                    if (SELECT_CASE_var1 == CoolingMode) {
                        DRink(SysNum).RefrigInletTemp = Node(DRink(SysNum).RefrigInNode).Temp;
                        DRink(SysNum).RefrigOutletTemp = Node(DRink(SysNum).RefrigOutNode).Temp;
                        DRink(SysNum).RefrigMassFlow = Node(DRink(SysNum).RefrigInNode).MassFlowRate;
                        DRink(SysNum).CoolPower = -TotalRadSysPower;
                    } else { // Not Operating: Leave temperatures at previous values
                        DRink(SysNum).RefrigMassFlow = 0.0;
                        DRink(SysNum).RefrigOutletTemp = DRink(SysNum).RefrigInletTemp;
                    }
                }

                DRink(SysNum).CoolEnergy = DRink(SysNum).CoolPower * TimeStepSys * SecInHour;
            } else if (SELECT_CASE_var == IndirectSystem) {
                for (Rink = 1; Rink <= NumOfIndirectRefrigSys; ++Rink) {
                    for (SurfNum = 1; SurfNum <= IRink(SysNum).NumOfSurfaces; ++SurfNum) {
                        if (Surface(IRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                            SurfFloor = IRink(SysNum).SurfacePtrArray(SurfNum);
                            TotalRadSysPower += QRadSysSource(SurfFloor);
                        }
                    }
                }
                IRink(SysNum).CoolPower = 0.0;

                {
                    auto const SELECT_CASE_var1(OperatingMode);

                    if (SELECT_CASE_var1 == CoolingMode) {
                        IRink(SysNum).RefrigInletTemp = Node(IRink(SysNum).RefrigInNode).Temp;
                        IRink(SysNum).RefrigOutletTemp = Node(IRink(SysNum).RefrigOutNode).Temp;
                        IRink(SysNum).RefrigMassFlow = Node(IRink(SysNum).RefrigInNode).MassFlowRate;
                        IRink(SysNum).CoolPower = -TotalRadSysPower;
                    } else { // Not Operating: Leave temperatures at previous values
                        IRink(SysNum).RefrigMassFlow = 0.0;
                        IRink(SysNum).RefrigOutletTemp = IRink(SysNum).RefrigInletTemp;
                    }
                }

                IRink(SysNum).CoolEnergy = IRink(SysNum).CoolPower * TimeStepSys * SecInHour;
            }

        }
    }

} // namespace IceRink
} // namespace EnergyPlus
