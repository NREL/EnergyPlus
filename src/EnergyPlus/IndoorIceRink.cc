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

    // Operating Mode:
    int NotOperating(0); // Parameter for use with OperatingMode variable, set for not operating
    int CoolingMode(2);  // Parameter for use with OperatingMode variable, set for cooling

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
    int OperatingMode(0);          // Used to keep track of whether system is in heating or cooling mode
    Array1D<Real64> ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
    Array1D<Real64> QRadSysSrcAvg;        // Average source over the time step for a particular radiant surface

    // Object Data
    Array1D<DirectRefrigSysData> DRink;
    Array1D<IndirectRefrigSysData> IRink;
    Array1D<RefrigSysTypeData> RefrigSysTypes;
    Array1D<ResurfacerData> Resurfacer;

    // Functions:

   /* void SimIndoorIceRink(std::string &Name,             // name of the refrigeration system
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
    }*/

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
        }

        Alphas.deallocate();
        Numbers.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();
    }

    Real64 IceRinkResurfacer(Real64 ResurfacerTank_capacity,  // Resurfacing machine tank capacity
                             Real64 ResurfacingHWTemperature, // Temperature of flood water
                             Real64 IceSurfaceTemperature,    // Temperature of ice rink surface
                             Real64 InitResurfWaterTemp,      // Initial temperature of resurfacing water
                             int const ResurfacerIndex)
    {
        static std::string const RoutineName("IceRinkResurfacer");
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        Real64 QResurfacing;  // Heat input(J) to the rink due to resurfacing events
        Real64 EHeatingWater; // Electric energy(J) required to heat water
        Real64 QHumidity;     // Heat input(J) to the ice rink due to humidity change during resurfacing events
        Real64 CpWater;
        // Real64 DeltaHWaterToIce;
        Real64 RhoWater;
        Real64 QFusion(333.55);
        Real64 CpIce(2.108);
        Real64 MolarMassWater(18.015);
        Real64 T_air_preResurfacing;
        Real64 T_air_postResurfacing;
        Real64 RH_air_preResurfacing;
        Real64 RH_air_postResurfacing;
        Real64 VolumeRink;
        Real64 DeltaT_ice;
        Real64 DeltaAH_ice;
        Real64 AH_preResurfacing;
        Real64 AH_postResurfacing;

        RhoWater = GetDensityGlycol(fluidNameWater, ResurfacingHWTemperature, Resurfacer(ResurfacerIndex).GlycolIndex, RoutineName);
        CpWater = GetSpecificHeatGlycol(fluidNameWater, ResurfacingHWTemperature, Resurfacer(ResurfacerIndex).GlycolIndex, RoutineName);
        QResurfacing = RhoWater * ResurfacerTank_capacity * ((CpWater * ResurfacingHWTemperature) + (QFusion) - (CpIce * IceSurfaceTemperature));
        EHeatingWater = ResurfacerTank_capacity * RhoWater * CpWater * (ResurfacingHWTemperature - InitResurfWaterTemp);

        T_air_preResurfacing = IceSurfaceTemperature;
        T_air_postResurfacing = ResurfacingHWTemperature;
        RH_air_preResurfacing = 0;
        RH_air_postResurfacing = 1;
        DeltaT_ice = abs(IceSurfaceTemperature - ResurfacingHWTemperature);
        VolumeRink = DRink(1).LengthRink * DRink(1).WidthRink * DRink(1).DepthRink;
        AH_preResurfacing = ((6.112 * exp((17.67 * T_air_preResurfacing) / (T_air_preResurfacing + 243.5)) * RH_air_preResurfacing * MolarMassWater) /
                             (100 * 0.08314 * (273.15 + T_air_preResurfacing))) *
                            (1 / RhoWater);
        AH_postResurfacing =
            ((6.112 * exp((17.67 * T_air_postResurfacing) / (T_air_postResurfacing + 243.5)) * RH_air_postResurfacing * MolarMassWater) /
             (100 * 0.08314 * (273.15 + T_air_postResurfacing))) *
            (1 / RhoWater);
        DeltaAH_ice = abs(AH_preResurfacing - AH_postResurfacing);
        QHumidity = DeltaAH_ice * VolumeRink * DeltaT_ice * CpWater;

        return QResurfacing;
        // return EHeatingWater;
        // return QHumidity;
    }

    Real64 CalcDRinkHXEffectTerm(Real64 const Temperature,    // Temperature of refrigerant entering the radiant system, in C
                                 int const SysNum,            // Index to the refrigeration system
                                 Real64 const RefrigMassFlow, // Mass flow rate of refrigerant in direct refrigeration system, kg/s
                                 Real64 TubeLength,           // Total length of the piping used in the radiant system
                                 Real64 TubeDiameter)
    {
        // Using/Aliasing
        using DataGlobals::Pi;
        using DataPlant::PlantLoop;
        using FluidProperties::GetSpecificHeatGlycol;

        // Return Value
        Real64 CalcDRinkHXEffectTerm;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const MaxLaminarRe(2300.0); // Maximum Reynolds number for laminar flow
        int const NumOfPropDivisions(13);
        Real64 const MaxExpPower(50.0); // Maximum power after which EXP argument would be zero for DP variables
        static Array1D<Real64> const Temps(NumOfPropDivisions,
                                           {-10.00, -9.00, -8.00, -7.00, -6.00, -5.00, -4.00, -3.00, -2.00, -1.00, 0.00, 1.00, 2.00});
        static Array1D<Real64> const Mu(NumOfPropDivisions,
                                        {0.000008843,
                                         0.000008878,
                                         0.000008913,
                                         0.000008947,
                                         0.000008982,
                                         0.000009017,
                                         0.000009052,
                                         0.000009087,
                                         0.000009123,
                                         0.000009158,
                                         0.000009193,
                                         0.000009228,
                                         0.000009264});

        static Array1D<Real64> const Conductivity(
            NumOfPropDivisions, {0.02224, 0.0223, 0.02237, 0.02243, 0.0225, 0.02257, 0.02264, 0.02271, 0.02277, 0.02285, 0.02292, 0.02299, 0.02306});
        static Array1D<Real64> const Pr(NumOfPropDivisions,
                                        {0.8741, 0.8741, 0.8741, 0.8741, 0.8741, 0.8741, 0.8741, 0.8742, 0.8742, 0.8742, 0.8743, 0.8744, 0.8744});
        static std::string const RoutineName("CalcDRinkHXEffectTerm");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Index;
        Real64 InterpFrac;
        Real64 NuD;
        Real64 ReD;
        Real64 NTU;
        Real64 CpRefrig(0.0);
        Real64 Kactual;
        Real64 MUactual;
        Real64 PRactual;
        Real64 Eff; // HX effectiveness

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

        CpRefrig = 2200;

        // Claculate the reynold's number
        ReD = 4.0 * RefrigMassFlow / (Pi * MUactual * TubeDiameter);

        if (ReD >= MaxLaminarRe) { // Turbulent flow --> use Colburn equation

            NuD = 0.023 * std::pow(ReD, 0.8) * std::pow(PRactual, 1.0 / 3.0);

        } else { // Laminar flow --> use constant surface temperature relation

            NuD = 3.66;
        }

        NTU = Pi * Kactual * NuD * TubeLength / (RefrigMassFlow * CpRefrig);
        if (NTU > MaxExpPower) {
            Eff = 1.0;
            CalcDRinkHXEffectTerm = RefrigMassFlow * CpRefrig;
        } else {
            Eff = 1.0 - std::exp(-NTU);
            CalcDRinkHXEffectTerm = Eff * RefrigMassFlow * CpRefrig;
        }

        return CalcDRinkHXEffectTerm;
    }

    Real64 CalcIRinkHXEffectTerm(Real64 const Temperature,    // Temperature of the refrigerant entering the radiant system
                                 int const SysNum,            // Index to the refrigeration system
                                 Real64 const RefrigMassFlow, // Mass flow rate of refrigerant in direct refrigeration system, kg/s
                                 Real64 TubeLength,           // Total length of the piping used in the radiant system
                                 Real64 TubeDiameter,         // Inner diameter of the piping used in the radiant system
                                 int const RefrigType, // Refrigerant used in the radiant system: Ethylene Glycol(EG) or Cslcium Chloride(CaCl2)
                                 Real64 Concentration  // Concentration of the brine(refrigerant) in the radiant system (allowed range 10% to 30%)
    )
    {
        // Using/Aliasing
        using DataGlobals::Pi;
        using DataPlant::PlantLoop;
        using FluidProperties::GetSpecificHeatGlycol;

        // Return Value
        Real64 CalcIRinkHXEffectTerm;

        Real64 const MaxLaminarRe(2300.0); // Maximum Reynolds number for laminar flow
        int const NumOfConcDivisions(3);   // Number of Concentration divisions (i.e the number of concentration data points)
        int const NumOfTempDivisions(13);  // Number of Temperature divisions (i.e the number of temperature data points)
        Real64 const MaxExpPower(50.0);    // Maximum power after which EXP argument would be zero for DP variables

        // Number of Data points for brines ( Calcium Chloride and Ethylene Glycol solution
        static Array1D<Real64> const Temperatures(NumOfTempDivisions,
                                                  {-10.00, -9.00, -8.00, -7.00, -6.00, -5.00, -4.00, -3.00, -2.00, -1.00, 0.00, 1.00, 2.00});

        // Properties of Calcium chloride  solution at 10%, 20% and 30% concentration
        // Conductivity (Watt/m-C)
        static Array1D<Real64> const ConductivityCacl2_C10(
            NumOfTempDivisions, {0.5395, 0.5411, 0.5427, 0.5442, 0.5458, 0.5474, 0.5489, 0.5505, 0.5521, 0.5537, 0.5552, 0.5568, 0.5584});
        static Array1D<Real64> const ConductivityCacl2_C20(
            NumOfTempDivisions, {0.5312, 0.5326, 0.5341, 0.5355, 0.537, 0.5384, 0.5399, 0.5413, 0.5427, 0.5442, 0.5456, 0.5471, 0.5485});
        static Array1D<Real64> const ConductivityCacl2_C30(
            NumOfTempDivisions, {0.5189, 0.5203, 0.5217, 0.5231, 0.5245, 0.5259, 0.5273, 0.5287, 0.5301, 0.5315, 0.5329, 0.5343, 0.5357});
        // Viscousity
        static Array1D<Real64> const MuCacl2_C10(
            NumOfTempDivisions,
            {0.003119, 0.003009, 0.002904, 0.002805, 0.00271, 0.002619, 0.002533, 0.002451, 0.002373, 0.002298, 0.002227, 0.002159, 0.002094});
        static Array1D<Real64> const MuCacl2_C20(
            NumOfTempDivisions,
            {0.004336, 0.004196, 0.004063, 0.003935, 0.003813, 0.003696, 0.003585, 0.003478, 0.003375, 0.003277, 0.003183, 0.003093, 0.003006});
        static Array1D<Real64> const MuCacl2_C30(
            NumOfTempDivisions,
            {0.007627, 0.00737, 0.007127, 0.006896, 0.006677, 0.006469, 0.006272, 0.006084, 0.005905, 0.005734, 0.005572, 0.005417, 0.005269});
        // Prandlt Number
        static Array1D<Real64> const PrCacl2_C10(NumOfTempDivisions,
                                                 {20.54, 19.76, 19.03, 18.33, 17.67, 17.04, 16.44, 15.87, 15.32, 14.81, 14.32, 13.85, 13.4});
        static Array1D<Real64> const PrCacl2_C20(NumOfTempDivisions,
                                                 {24.68, 23.85, 23.05, 22.29, 21.56, 20.87, 20.21, 19.57, 18.96, 18.38, 17.83, 17.29, 16.78});
        static Array1D<Real64> const PrCacl2_C30(NumOfTempDivisions,
                                                 {39.59, 38.19, 36.86, 35.6, 34.41, 33.28, 32.2, 31.18, 30.21, 29.29, 28.41, 27.57, 26.77});

        // Properties of Ethylene Glycol solution at 10%, 20% and 30% concentration
        // Conductivity (Watt/m-C)
        static Array1D<Real64> const ConductivityEG_C10(
            NumOfTempDivisions, {0.5069, 0.5086, 0.5103, 0.5119, 0.5136, 0.5152, 0.5169, 0.5185, 0.5201, 0.5217, 0.5233, 0.5249, 0.5264});
        static Array1D<Real64> const ConductivityEG_C20(
            NumOfTempDivisions, {0.4716, 0.4729, 0.4742, 0.4754, 0.4767, 0.4779, 0.4792, 0.4804, 0.4817, 0.4829, 0.4841, 0.4854, 0.4866});
        static Array1D<Real64> const ConductivityEG_C30(
            NumOfTempDivisions, {0.4362, 0.4371, 0.4381, 0.4391, 0.4401, 0.4411, 0.442, 0.443, 0.444, 0.445, 0.4459, 0.4469, 0.4479});
        // Viscousity
        static Array1D<Real64> const MuEG_C10(
            NumOfTempDivisions,
            {0.003395, 0.003265, 0.003141, 0.003023, 0.002911, 0.002805, 0.002704, 0.002607, 0.002516, 0.002428, 0.002345, 0.002265, 0.00219});
        static Array1D<Real64> const MuEG_C20(
            NumOfTempDivisions,
            {0.004699, 0.004509, 0.004328, 0.004157, 0.003995, 0.003841, 0.003694, 0.003555, 0.003423, 0.003297, 0.003178, 0.003064, 0.002955});
        static Array1D<Real64> const MuEG_C30(
            NumOfTempDivisions,
            {0.006508, 0.006228, 0.005964, 0.005715, 0.005478, 0.005254, 0.005042, 0.004841, 0.00465, 0.004469, 0.004298, 0.004135, 0.00398});
        // Prandlt Number
        static Array1D<Real64> const PrEG_C10(NumOfTempDivisions,
                                              {27.04, 25.91, 24.85, 23.84, 22.88, 21.98, 21.12, 20.3, 19.53, 18.79, 18.09, 17.43, 16.8});
        static Array1D<Real64> const PrEG_C20(NumOfTempDivisions,
                                              {38.3, 36.67, 35.12, 33.66, 32.27, 30.96, 29.71, 28.53, 27.41, 26.35, 25.34, 24.38, 23.47});
        static Array1D<Real64> const PrEG_C30(NumOfTempDivisions,
                                              {54.12, 51.72, 49.46, 47.32, 45.3, 43.39, 41.58, 39.87, 38.25, 36.71, 35.25, 33.87, 32.56});

        static std::string const RoutineName("CalcIRinkHXEffectTerm");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Index;
        Real64 InterpFrac;
        Real64 NuD;
        Real64 ReD;
        Real64 NTU;
        Real64 CpCacl2(0.0);
        Real64 CpEG(0.0);
        Real64 Kactual;
        Real64 MUactual;
        Real64 PRactual;
        Real64 Eff; // HX effectiveness

        // First find out where we are in the range of temperatures
        Index = 1;
        while (Index <= NumOfTempDivisions) {
            if (Temperature < Temperatures(Index)) break; // DO loop
            ++Index;
        }

        if (RefrigType == CaCl2) {
            // Initialize thermal properties of Calcium Chloride Solution
            if (Concentration == 10.00) {
                if (Index == 1) {
                    MUactual = MuCacl2_C10(Index);
                    Kactual = ConductivityCacl2_C10(Index);
                    PRactual = PrCacl2_C10(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuCacl2_C10(Index);
                    Kactual = ConductivityCacl2_C10(Index);
                    PRactual = PrCacl2_C10(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuCacl2_C10(Index - 1) + InterpFrac * (MuCacl2_C10(Index) - MuCacl2_C10(Index - 1));
                    Kactual = ConductivityCacl2_C10(Index - 1) + InterpFrac * (ConductivityCacl2_C10(Index) - ConductivityCacl2_C10(Index - 1));
                    PRactual = PrCacl2_C10(Index - 1) + InterpFrac * (PrCacl2_C10(Index) - PrCacl2_C10(Index - 1));
                }
            } else if (Concentration == 20.00) {
                if (Index == 1) {
                    MUactual = MuCacl2_C20(Index);
                    Kactual = ConductivityCacl2_C20(Index);
                    PRactual = PrCacl2_C20(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuCacl2_C20(Index);
                    Kactual = ConductivityCacl2_C20(Index);
                    PRactual = PrCacl2_C20(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuCacl2_C20(Index - 1) + InterpFrac * (MuCacl2_C20(Index) - MuCacl2_C20(Index - 1));
                    Kactual = ConductivityCacl2_C20(Index - 1) + InterpFrac * (ConductivityCacl2_C20(Index) - ConductivityCacl2_C20(Index - 1));
                    PRactual = PrCacl2_C20(Index - 1) + InterpFrac * (PrCacl2_C20(Index) - PrCacl2_C20(Index - 1));
                }

            } else {
                if (Index == 1) {
                    MUactual = MuCacl2_C30(Index);
                    Kactual = ConductivityCacl2_C30(Index);
                    PRactual = PrCacl2_C30(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuCacl2_C30(Index);
                    Kactual = ConductivityCacl2_C30(Index);
                    PRactual = PrCacl2_C30(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuCacl2_C30(Index - 1) + InterpFrac * (MuCacl2_C30(Index) - MuCacl2_C30(Index - 1));
                    Kactual = ConductivityCacl2_C30(Index - 1) + InterpFrac * (ConductivityCacl2_C30(Index) - ConductivityCacl2_C30(Index - 1));
                    PRactual = PrCacl2_C30(Index - 1) + InterpFrac * (PrCacl2_C30(Index) - PrCacl2_C30(Index - 1));
                }
            }
        } else if (RefrigType == EG) {
            // Initialize thermal properties of Ethylene Glycol Solution
            if (Concentration == 10.00) {
                if (Index == 1) {
                    MUactual = MuEG_C10(Index);
                    Kactual = ConductivityEG_C10(Index);
                    PRactual = PrEG_C10(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuEG_C10(Index);
                    Kactual = ConductivityEG_C10(Index);
                    PRactual = PrEG_C10(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuEG_C10(Index - 1) + InterpFrac * (MuEG_C10(Index) - MuEG_C10(Index - 1));
                    Kactual = ConductivityEG_C10(Index - 1) + InterpFrac * (ConductivityEG_C10(Index) - ConductivityEG_C10(Index - 1));
                    PRactual = PrEG_C10(Index - 1) + InterpFrac * (PrEG_C10(Index) - PrEG_C10(Index - 1));
                }
            } else if (Concentration == 20.00) {
                if (Index == 1) {
                    MUactual = MuEG_C20(Index);
                    Kactual = ConductivityEG_C20(Index);
                    PRactual = PrEG_C20(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuEG_C20(Index);
                    Kactual = ConductivityEG_C20(Index);
                    PRactual = PrEG_C20(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuEG_C20(Index - 1) + InterpFrac * (MuEG_C20(Index) - MuEG_C20(Index - 1));
                    Kactual = ConductivityEG_C20(Index - 1) + InterpFrac * (ConductivityEG_C20(Index) - ConductivityEG_C20(Index - 1));
                    PRactual = PrEG_C20(Index - 1) + InterpFrac * (PrEG_C20(Index) - PrEG_C20(Index - 1));
                }

            } else {
                if (Index == 1) {
                    MUactual = MuEG_C30(Index);
                    Kactual = ConductivityEG_C30(Index);
                    PRactual = PrEG_C30(Index);
                } else if (Index > NumOfTempDivisions) {
                    Index = NumOfTempDivisions;
                    MUactual = MuEG_C30(Index);
                    Kactual = ConductivityEG_C30(Index);
                    PRactual = PrEG_C30(Index);
                } else {
                    InterpFrac = (Temperature - Temperatures(Index - 1)) / (Temperatures(Index) - Temperatures(Index - 1));
                    MUactual = MuEG_C30(Index - 1) + InterpFrac * (MuEG_C30(Index) - MuEG_C30(Index - 1));
                    Kactual = ConductivityEG_C30(Index - 1) + InterpFrac * (ConductivityEG_C30(Index) - ConductivityEG_C30(Index - 1));
                    PRactual = PrEG_C30(Index - 1) + InterpFrac * (PrEG_C30(Index) - PrEG_C30(Index - 1));
                }
            }
        }

        CpCacl2 = 2700;
        CpEG = 3600;

        // Claculate the reynold's number
        ReD = 4.0 * RefrigMassFlow / (Pi * MUactual * TubeDiameter);

        if (ReD >= MaxLaminarRe) { // Turbulent flow --> use Colburn equation

            NuD = 0.023 * std::pow(ReD, 0.8) * std::pow(PRactual, 1.0 / 3.0);

        } else { // Laminar flow --> use constant surface temperature relation

            NuD = 3.66;
        }

        if (RefrigType == CaCl2) {
            NTU = Pi * Kactual* NuD * TubeLength / (RefrigMassFlow * CpCacl2);
        } else {
            NTU = Pi * Kactual * NuD * TubeLength / (RefrigMassFlow * CpEG);
        }

        if (NTU > MaxExpPower) {
            Eff = 1.0;
            if (RefrigType == CaCl2) {
                CalcIRinkHXEffectTerm = RefrigMassFlow * CpCacl2;
            } else {
                CalcIRinkHXEffectTerm = RefrigMassFlow * CpEG;
            }
        } else {
            Eff = 1.0 - std::exp(-NTU);
            if (RefrigType == CaCl2) {
                CalcIRinkHXEffectTerm = Eff * RefrigMassFlow * CpCacl2;
            } else {
                CalcIRinkHXEffectTerm = Eff * RefrigMassFlow * CpEG;
            }
        }

        return CalcIRinkHXEffectTerm;
    }

    void CalcDirectIndoorIceRinkComps(int const SysNum, // Index number for the indirect refrigeration system
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
        using DataEnvironment::OutBaroPress;
        using DataHeatBalance::Zone;
        using DataHeatBalFanSys::CTFTsrcConstPart;
        using DataHeatBalFanSys::RadSysTiHBConstCoef;
        using DataHeatBalFanSys::RadSysTiHBQsrcCoef;
        using DataHeatBalFanSys::RadSysTiHBToutCoef;
        using DataHeatBalFanSys::RadSysToHBConstCoef;
        using DataHeatBalFanSys::RadSysToHBQsrcCoef;
        using DataHeatBalFanSys::RadSysToHBTinCoef;
        using DataHeatBalFanSys::ZoneAirHumRat;
        using DataHeatBalSurface::TH;
        using DataLoopNode::Node;
        using DataSurfaces::HeatTransferModel_CTF;
        using DataSurfaces::SurfaceClass_Floor;
        using General::RoundSigDigits;
        using PlantUtilities::SetComponentFlowRate;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CondSurfNum;           // Surface number (in radiant array) of
        int RefrigNodeIn;          // Node number of the refrigerant entering the refrigeration system
        int ZoneNum;               // Zone pointer for this refrigeration system
        int SurfNum;               // DO loop counter for the surfaces that comprise a particular refrigeration system
        int SurfNum2;              // Index to the floor in the surface derived type
        int SurfNumA;              // DO loop counter for the surfaces that comprise a particular refrigeration system
        int SurfNumB;              // DO loop counter for the surfaces that comprise a particular refrigeration system 
        int SurfNum2A;             // Index to the floor in the surface derived type
        int SurfNumC;              // Index to surface for condensation control 
        int ConstrNum;             // Index for construction number in Construct derived type
        Real64 SysRefrigMassFlow;  // System level refrigerant mass flow rate (includes effect of zone multiplier)
        Real64 RefrigMassFlow;     // Refrigerant mass flow rate in the refrigeration system, kg/s
        Real64 RefrigTempIn;       // Temperature of the refrigerant entering the refrigeration system, in C
        Real64 EpsMdotCp;          // Epsilon (heat exchanger terminology) times refrigerant mass flow rate times water specific heat
        Real64 DewPointTemp;       // Dew-point temperature based on the zone air conditions
        Real64 LowestRadSurfTemp;  // Lowest surface temperature of a radiant system (when condensation is a concern)
        Real64 FullRefrigMassFlow; // Original refrigerant mass flow rate before reducing the flow for condensation concerns
        Real64 PredictedCondTemp;  // Temperature at which condensation is predicted (includes user parameter)
        Real64 ZeroFlowSurfTemp;   // Temperature of radiant surface when flow is zero
        Real64 ReductionFrac;      // Fraction that the flow should be reduced to avoid condensation

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

        RefrigNodeIn = DRink(SysNum).ColdRefrigInNode;
        if (RefrigNodeIn == 0) {
            ShowSevereError("Illegal inlet node for the refrigerant in the direct system");
            ShowFatalError("Preceding condition causes termination");
        }

        ZoneNum = DRink(SysNum).ZonePtr;
        SysRefrigMassFlow = Node(RefrigNodeIn).MassFlowRate;
        RefrigMassFlow = Node(RefrigNodeIn).MassFlowRate / double(Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier);
        RefrigTempIn = Node(RefrigNodeIn).Temp;

        if (RefrigMassFlow <= 0) {
            // No flow or below minimum allowed so there is no heat source/sink
            // This is possible with a mismatch between system and plant operation
            // or a slight mismatch between zone and system controls.  This is not
            // necessarily a "problem" so this exception is necessary in the code.
            for (SurfNum = 1; SurfNum <= DRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                    SurfNum2 = DRink(SysNum).SurfacePtrArray(SurfNum);
                    QRadSysSource(SurfNum2) = 0.0;
                    if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                        QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                }
            }
        } else {    // Refrigerant mass flow rate is significant
            for (SurfNum = 1; SurfNum <= DRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                    SurfNum2 = DRink(SysNum).SurfacePtrArray(SurfNum);
                    // Determine the heat exchanger "effectiveness" term
                    EpsMdotCp = CalcDRinkHXEffectTerm(RefrigTempIn, 
                                                      SysNum, 
                                                      RefrigMassFlow, 
                                                      DRink(SysNum).TubeLength, 
                                                      DRink(SysNum).TubeLength);

                    ConstrNum = Surface(SurfNum2).Construction;
                    if (Surface(SurfNum2).HeatTransferAlgorithm == HeatTransferModel_CTF) {

                        Ca = RadSysTiHBConstCoef(SurfNum2);
                        Cb = RadSysTiHBToutCoef(SurfNum2);
                        Cc = RadSysTiHBQsrcCoef(SurfNum2);

                        Cd = RadSysToHBConstCoef(SurfNum2);
                        Ce = RadSysToHBTinCoef(SurfNum2);
                        Cf = RadSysToHBQsrcCoef(SurfNum2);

                        Cg = CTFTsrcConstPart(SurfNum2);
                        Ch = Construct(ConstrNum).CTFTSourceQ(0);
                        Ci = Construct(ConstrNum).CTFTSourceIn(0);
                        Cj = Construct(ConstrNum).CTFTSourceOut(0);

                        Ck = Cg + ((Ci * (Ca + Cb * Cd) + Cj * (Cd + Ce * Ca)) / (1.0 - Ce * Cb));
                        Cl = Ch + ((Ci * (Cc + Cb * Cf) + Cj * (Cf + Ce * Cc)) / (1.0 - Ce * Cb));

                        QRadSysSource(SurfNum2) = EpsMdotCp * (RefrigTempIn - Ck) / (1.0 + (EpsMdotCp * Cl / Surface(SurfNum2).Area));
                    }

                    if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                        QRadSysSource(Surface(SurfNum2).ExtBoundCond) = QRadSysSource(SurfNum2); // Also set the other side of an interzone
                }
            }

            // "Temperature Comparision" Cut-off:
            for (SurfNum = 1; SurfNum <= DRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                    // Check to see whether or not the system should really be running.  If
                    // QRadSysSource is positive when we are in cooling mode, then the radiant system
                    // will be doing the opposite of its intention.  In this case, the flow rate
                    // is set to zero to avoid heating in cooling mode
                    SurfNum2 = DRink(SysNum).SurfacePtrArray(SurfNum);
                    if ((OperatingMode == CoolingMode) && (QRadSysSource(SurfNum) >= 0.0)) {
                        RefrigMassFlow = 0.0;
                        SetComponentFlowRate(RefrigMassFlow,
                                             DRink(SysNum).ColdRefrigInNode,
                                             DRink(SysNum).ColdRefrigOutNode,
                                             DRink(SysNum).CRefrigLoopNum,
                                             DRink(SysNum).CRefrigLoopSide,
                                             DRink(SysNum).CRefrigBranchNum,
                                             DRink(SysNum).CRefrigCompNum);
                    }
                    DRink(SysNum).RefrigMassFlowRate = RefrigMassFlow;

                    for (SurfNumA = 1; SurfNumA <= DRink(SysNum).NumOfSurfaces; ++SurfNumA) {
                        if (Surface(DRink(SysNum).SurfacePtrArray(SurfNumA)).Class == SurfaceClass_Floor) {
                        SurfNum2A = DRink(SysNum).SurfacePtrArray(SurfNumA);
                        QRadSysSource(SurfNum2A) = 0.0;
                        if (Surface(SurfNum2A).ExtBoundCond > 0 && Surface(SurfNum2A).ExtBoundCond != SurfNum2A)
                            QRadSysSource(Surface(SurfNum2A).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                        }
                    }
                    break;
                }
            }

            // Condensation Cut-off:
            // Check to see whether the ice floor temperature operated by the refrigeration system that have
            // dropped below the dew-point temperature.  If so, we need to shut off this refrigeration system.
            // A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
            // conditions.
            DRink(SysNum).CondCausedShutDown = false;
            DewPointTemp = PsyTdpFnWPb(ZoneAirHumRat(ZoneNum), OutBaroPress);

            if ((OperatingMode == CoolingMode) && (DRink(SysNum).CondCtrlType == CondCtrlSimpleOff)) {
                for (SurfNumC = 1; SurfNumC <= DRink(SysNum).NumOfSurfaces; ++SurfNumC) {
                    if (Surface(DRink(SysNum).SurfacePtrArray(SurfNumC)).Class == SurfaceClass_Floor) {
                    
                        if (TH(2, 1, DRink(SysNum).SurfacePtrArray(SurfNumC)) < (DewPointTemp + DRink(SysNum).CondDewPtDeltaT)) {
                            // Condensation warning--must shut off refrigeration system
                            DRink(SysNum).CondCausedShutDown = true;
                            RefrigMassFlow = 0.0;
                            SetComponentFlowRate(RefrigMassFlow,
                                                 DRink(SysNum).ColdRefrigInNode,
                                                 DRink(SysNum).ColdRefrigOutNode,
                                                 DRink(SysNum).CRefrigLoopNum,
                                                 DRink(SysNum).CRefrigLoopSide,
                                                 DRink(SysNum).CRefrigBranchNum,
                                                 DRink(SysNum).CRefrigCompNum);
                            DRink(SysNum).RefrigMassFlowRate = RefrigMassFlow;
                            for (SurfNumB = 1; SurfNumB <= DRink(SysNum).NumOfSurfaces; ++SurfNumB) {
                                if (Surface(DRink(SysNum).SurfacePtrArray(SurfNumB)).Class == SurfaceClass_Floor) {
                                    SurfNum2A = DRink(SysNum).SurfacePtrArray(SurfNumB);
                                    QRadSysSource(SurfNum2A) = 0.0;
                                    if (Surface(SurfNum2A).ExtBoundCond > 0 && Surface(SurfNum2A).ExtBoundCond != SurfNum2A)
                                        QRadSysSource(Surface(SurfNum2A).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                                }
                            }
                            // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                            if (!WarmupFlag) {
                                if (DRink(SysNum).CondErrIndex == 0) {
                                    ShowWarningMessage(cDRink + " [" + DRink(SysNum).Name + ']');
                                    ShowContinueError("Surface [" + Surface(DRink(SysNum).SurfacePtrArray(SurfNumC)).Name +
                                                      "] temperature below dew-point temperature--potential for condensation exists");
                                    ShowContinueError("Flow to the radiant system will be shut-off to avoid condensation");
                                    ShowContinueError("Predicted radiant system surface temperature = " +
                                                      RoundSigDigits(TH(2, 1, DRink(SysNum).SurfacePtrArray(SurfNumC)), 2));
                                    ShowContinueError("Zone dew-point temperature + safety delta T= " +
                                                      RoundSigDigits(DewPointTemp + DRink(SysNum).CondDewPtDeltaT, 2));
                                    ShowContinueErrorTimeStamp("");
                                    ShowContinueError("Note that a " + RoundSigDigits(DRink(SysNum).CondDewPtDeltaT, 4) +
                                                      " C safety was chosen in the input for the shut-off criteria");
                                }
                                ShowRecurringWarningErrorAtEnd(cDRink + " [" + DRink(SysNum).Name + "] condensation shut-off occurrence continues.",
                                                               DRink(SysNum).CondErrIndex,
                                                               DewPointTemp,
                                                               DewPointTemp,
                                                               _,
                                                               "C",
                                                               "C");
                            }
                            break; // outer do loop
                        }
                    }
                }
            } else if ((OperatingMode == CoolingMode) && (DRink(SysNum).CondCtrlType == CondCtrlNone)) {
                for (SurfNumC = 1; SurfNumC <= DRink(SysNum).NumOfSurfaces; ++SurfNumC) {
                    if (Surface(DRink(SysNum).SurfacePtrArray(SurfNumC)).Class == SurfaceClass_Floor) {
                        if (TH(2, 1, DRink(SysNum).SurfacePtrArray(SurfNumC)) < DewPointTemp) {
                            // Condensation occuring but user does not want to shut down the system off ever
                            DRink(SysNum).CondCausedShutDown = true;
                        }
                    }
                }
            } else if ((OperatingMode == CoolingMode) && (DRink(SysNum).CondCtrlType == CondCtrlVariedOff)) {
                LowestRadSurfTemp = 999.9;
                CondSurfNum = 0;
                for (SurfNumC = 1; SurfNumC <= DRink(SysNum).NumOfSurfaces; ++SurfNumC) {
                    if (Surface(DRink(SysNum).SurfacePtrArray(SurfNumC)).Class == SurfaceClass_Floor) {
                        if (TH(2, 1, DRink(SysNum).SurfacePtrArray(SurfNumC)) < (DewPointTemp + DRink(SysNum).CondDewPtDeltaT)) {
                            if (TH(2, 1, DRink(SysNum).SurfacePtrArray(SurfNumC)) < LowestRadSurfTemp) {
                                LowestRadSurfTemp = TH(2, 1, DRink(SysNum).SurfacePtrArray(SurfNumC));
                                CondSurfNum = SurfNumC;
                            }
                        }
                    }
                }

                if (CondSurfNum > 0) {
                    // Condensation predicted so let's deal with it
                    // Process here is: turn everything off and see what the resulting surface temperature is for
                    // the surface that was causing the lowest temperature.  Then, interpolate to find the flow that
                    // would still allow the system to operate without producing condensation.  Rerun the heat balance
                    // and recheck for condensation.  If condensation still exists, shut everything down.  This avoids
                    // excessive iteration and still makes an attempt to vary the flow rate.
                    // First, shut everything off...

                    FullRefrigMassFlow = RefrigMassFlow;
                    RefrigMassFlow = 0.0;
                    SetComponentFlowRate(RefrigMassFlow,
                                         DRink(SysNum).ColdRefrigInNode,
                                         DRink(SysNum).ColdRefrigOutNode,
                                         DRink(SysNum).CRefrigLoopNum,
                                         DRink(SysNum).CRefrigLoopSide,
                                         DRink(SysNum).CRefrigBranchNum,
                                         DRink(SysNum).CRefrigCompNum);
                    DRink(SysNum).RefrigMassFlowRate = RefrigMassFlow;
                    for (SurfNumB = 1; SurfNumB <= DRink(SysNum).NumOfSurfaces; ++SurfNumB) {
                        if (Surface(DRink(SysNum).SurfacePtrArray(SurfNumB)).Class == SurfaceClass_Floor) {
                            SurfNum2A = DRink(SysNum).SurfacePtrArray(SurfNumB);
                            QRadSysSource(SurfNum2A) = 0.0;
                            if (Surface(SurfNum2A).ExtBoundCond > 0 && Surface(SurfNum2A).ExtBoundCond != SurfNum2A)
                            QRadSysSource(Surface(SurfNum2A).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                    
                        }
                    }
                    // Redo the heat balances since we have changed the heat source (set it to zero)
                    HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(ZoneNum);
                    HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(ZoneNum);
                    // Now check the floor surface temperatures.  If any potentially have condensation, leave the system off.
                    for (SurfNum2 = 1; SurfNum2 <= DRink(SysNum).NumOfSurfaces; ++SurfNum2) {
                        if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum2)).Class == SurfaceClass_Floor) {
                        
                            if (TH(2, 1, DRink(SysNum).SurfacePtrArray(SurfNum2)) < (DewPointTemp + DRink(SysNum).CondDewPtDeltaT)) {
                                DRink(SysNum).CondCausedShutDown = true;
                            }
                        }
                    }
                    // If the system does not need to be shut down, then let's see if we can vary the flow based
                    // on the lowest temperature surface from before.  This will use interpolation to try a new
                    // flow rate.
                    if (!DRink(SysNum).CondCausedShutDown) {
                        PredictedCondTemp = DewPointTemp + DRink(SysNum).CondDewPtDeltaT;
                        ZeroFlowSurfTemp = TH(2, 1, DRink(SysNum).SurfacePtrArray(CondSurfNum));
                        ReductionFrac = (ZeroFlowSurfTemp - PredictedCondTemp) / std::abs(ZeroFlowSurfTemp - LowestRadSurfTemp);
                        if (ReductionFrac < 0.0) ReductionFrac = 0.0; // Shouldn't happen as the above check should have screened this out
                        if (ReductionFrac > 1.0) ReductionFrac = 1.0; // Shouldn't happen either because condensation doesn't exist then
                        RefrigMassFlow = ReductionFrac * FullRefrigMassFlow;
                        SysRefrigMassFlow = double(Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier) * RefrigMassFlow;
                        // Got a new reduced flow rate that should work...reset loop variable and resimulate the system
                        SetComponentFlowRate(SysRefrigMassFlow,
                                             DRink(SysNum).ColdRefrigInNode,
                                             DRink(SysNum).ColdRefrigOutNode,
                                             DRink(SysNum).CRefrigLoopNum,
                                             DRink(SysNum).CRefrigLoopSide,
                                             DRink(SysNum).CRefrigBranchNum,
                                             DRink(SysNum).CRefrigCompNum);
                        DRink(SysNum).RefrigMassFlowRate = SysRefrigMassFlow;
                        // Go through the floor surface again with the new flow rate...
                        for (SurfNumB = 1; SurfNumB <= DRink(SysNum).NumOfSurfaces; ++SurfNumB) {
                            if (Surface(DRink(SysNum).SurfacePtrArray(SurfNumB)).Class == SurfaceClass_Floor) {
                            
                                SurfNum = DRink(SysNum).SurfacePtrArray(SurfNumB);
                                // Determine the heat exchanger "effectiveness" term
                                EpsMdotCp =
                                    CalcDRinkHXEffectTerm(RefrigTempIn, SysNum, RefrigMassFlow, DRink(SysNum).TubeLength, DRink(SysNum).TubeLength);
                                if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CTF) {
                                    // For documentation on coefficients, see code earlier in this subroutine
                                    Ca = RadSysTiHBConstCoef(SurfNum);
                                    Cb = RadSysTiHBToutCoef(SurfNum);
                                    Cc = RadSysTiHBQsrcCoef(SurfNum);
                                    Cd = RadSysToHBConstCoef(SurfNum);
                                    Ce = RadSysToHBTinCoef(SurfNum);
                                    Cf = RadSysToHBQsrcCoef(SurfNum);
                                    Cg = CTFTsrcConstPart(SurfNum);
                                    Ch = Construct(ConstrNum).CTFTSourceQ(0);
                                    Ci = Construct(ConstrNum).CTFTSourceIn(0);
                                    Cj = Construct(ConstrNum).CTFTSourceOut(0);
                                    Ck = Cg + ((Ci * (Ca + Cb * Cd) + Cj * (Cd + Ce * Ca)) / (1.0 - Ce * Cb));
                                    Cl = Ch + ((Ci * (Cc + Cb * Cf) + Cj * (Cf + Ce * Cc)) / (1.0 - Ce * Cb));
                                    QRadSysSource(SurfNum) = EpsMdotCp * (RefrigTempIn - Ck) / (1.0 + (EpsMdotCp * Cl / Surface(SurfNum).Area));
                                }
                                if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum)
                                    QRadSysSource(Surface(SurfNum).ExtBoundCond) = QRadSysSource(SurfNum); // Also set the other side of an interzone
                        
                            }
                        }
                        // Redo the heat balances since we have changed the heat source
                        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(ZoneNum);
                        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(ZoneNum);

                        // Check for condensation one more time.  If no condensation, we are done.  If there is
                        // condensation, shut things down and be done.
                        for (SurfNum2 = 1; SurfNum2 <= DRink(SysNum).NumOfSurfaces; ++SurfNum2) {
                            if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum2)).Class == SurfaceClass_Floor) {
                                SurfNum = DRink(SysNum).SurfacePtrArray(SurfNum2);
                                if (DRink(SysNum).CondCausedShutDown) break;
                                if (TH(2, 1, DRink(SysNum).SurfacePtrArray(SurfNum)) < (PredictedCondTemp)) {
                                    // Condensation still present--must shut off radiant system
                                    DRink(SysNum).CondCausedShutDown = true;
                                    RefrigMassFlow = 0.0;
                                    
                                    SetComponentFlowRate(SysRefrigMassFlow,
                                                         DRink(SysNum).ColdRefrigInNode,
                                                         DRink(SysNum).ColdRefrigOutNode,
                                                         DRink(SysNum).CRefrigLoopNum,
                                                         DRink(SysNum).CRefrigLoopSide,
                                                         DRink(SysNum).CRefrigBranchNum,
                                                         DRink(SysNum).CRefrigCompNum);
                                    DRink(SysNum).RefrigMassFlowRate = RefrigMassFlow;
                                    for (SurfNumB = 1; SurfNumB <= DRink(SysNum).NumOfSurfaces; ++SurfNumB) {
                                        SurfNum2A = DRink(SysNum).SurfacePtrArray(SurfNumB);
                                        QRadSysSource(SurfNum2A) = 0.0;
                                        if (Surface(SurfNum2A).ExtBoundCond > 0 && Surface(SurfNum2A).ExtBoundCond != SurfNum2A)
                                            QRadSysSource(Surface(SurfNum2A).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                                    }
                                }
                            }
                        }
                    }

                    if (DRink(SysNum).CondCausedShutDown) {
                        // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                        if (!WarmupFlag) {
                            if (DRink(SysNum).CondErrIndex == 0) {
                                ShowWarningMessage(cDRink + " [" + DRink(SysNum).Name + ']');
                                ShowContinueError("Surface [" + Surface(DRink(SysNum).SurfacePtrArray(CondSurfNum)).Name +
                                                  "] temperature below dew-point temperature--potential for condensation exists");
                                ShowContinueError("Flow to the radiant system will be shut-off to avoid condensation");
                                ShowContinueError("Predicted radiant system surface temperature = " +
                                                  RoundSigDigits(TH(2, 1, DRink(SysNum).SurfacePtrArray(CondSurfNum)), 2));
                                ShowContinueError("Zone dew-point temperature + safety delta T= " +
                                                  RoundSigDigits(DewPointTemp + DRink(SysNum).CondDewPtDeltaT, 2));
                                ShowContinueErrorTimeStamp("");
                                ShowContinueError("Note that a " + RoundSigDigits(DRink(SysNum).CondDewPtDeltaT, 4) +
                                                  " C safety was chosen in the input for the shut-off criteria");
                            }
                            ShowRecurringWarningErrorAtEnd(cDRink + " [" + DRink(SysNum).Name + "] condensation shut-off occurrence continues.",
                                                           DRink(SysNum).CondErrIndex,
                                                           DewPointTemp,
                                                           DewPointTemp,
                                                           _,
                                                           "C",
                                                           "C");
                        }
                    }
                } // Condensation Predicted in Variable Shut-Off Control Type
            }     // In cooling mode and one of the condensation control types
        }         // There was a non-zero flow

        // Now that we have the source/sink term, we must redo the heat balances to obtain
        // the new SumHATsurf value for the zone.  Note that the difference between the new
        // SumHATsurf and the value originally calculated by the heat balance with a zero
        // source for all radiant systems in the zone is the load met by the system (approximately).
        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(ZoneNum);
        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(ZoneNum);

        LoadMet = SumHATsurf(ZoneNum) - ZeroSourceSumHATsurf(ZoneNum);
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
} // namespace IceRink
} // namespace EnergyPlus
