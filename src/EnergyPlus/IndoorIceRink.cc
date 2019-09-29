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
    int NumOfDirectRefrigSys(0);          // Number of direct refrigeration type ice rinks
    int NumOfIndirectRefrigSys(0);        // Number of indirect refrigeration type ice rinks
    int OperatingMode(0);                 // Used to keep track of whether system is in heating or cooling mode
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
        // Viscousity
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
        // Specific heat
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

        // Properties of Ethylene Glycol solution at 10%, 20% and 30% concentration
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
        // Specific heat
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

        static std::string const RoutineName("CalcIRinkHXEffectTerm");

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
        Real64 Eff; // HX effectiveness

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

            } else if (Concentration = 27.00) {
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
            } else if (Concentration = 28.00) {
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
            } else if (Concentration = 29.00) {
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
                Eff = 1.0;
                CalcIRinkHXEffectTerm = RefrigMassFlow * Cpactual;
                
                
            } else {
                Eff = 1.0 - std::exp(-NTU);
                CalcIRinkHXEffectTerm = Eff * RefrigMassFlow * Cpactual;
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

            } else if (Concentration = 27.00) {
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
            } else if (Concentration = 28.00) {
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
            } else if (Concentration = 29.00) {
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
                Eff = 1.0;
                CalcIRinkHXEffectTerm = RefrigMassFlow * Cpactual;

            } else {
                Eff = 1.0 - std::exp(-NTU);
                CalcIRinkHXEffectTerm = Eff * RefrigMassFlow * Cpactual;
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
        } else { // Refrigerant mass flow rate is significant
            for (SurfNum = 1; SurfNum <= DRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                    SurfNum2 = DRink(SysNum).SurfacePtrArray(SurfNum);
                    // Determine the heat exchanger "effectiveness" term
                    EpsMdotCp = CalcDRinkHXEffectTerm(RefrigTempIn, SysNum, RefrigMassFlow, DRink(SysNum).TubeLength, DRink(SysNum).TubeLength);

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

    void CalcDirectIndoorIceRinkSys(int const SysNum, // name of the direct refrigeration system
                                    Real64 &LoadMet   // load met by the direct refrigeration system, in Watts
    )
    {
        // Using/Aliasing
        using DataBranchAirLoopPlant::MassFlowTolerance;
        using DataSurfaces::SurfaceClass_Floor;
        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum;          // Surface number in the Surface derived type for a radiant system surface
        int SurfNum2;         // Surface number in the Surface derived type for a radiant system surface
        Real64 mdot;          // local temporary for fluid mass flow rate
        Real64 ControlTemp;   // temperature of whatever is controlling the refrigerant system
        Real64 SetPointTemp;  // temperature "goal" for the refrigerant system [Celsius]
        Real64 OffTempCool;   // temperature at which the flow rate throttles back to zero for cooling
        int ControlNode;      // the cold refrigerant inlet node
        Real64 MassFlowFrac;  // fraction of the maximum refrigerant flow rate as determined by the control algorithm
        Real64 MaxRefrigFlow; // maximum refrigerant flow for heating or cooling [kg/sec]
        Real64 ActRefrigFlow; // actual refrigerant flow for cooling
        bool SysRunning;      // True when system is running

        ControlNode = 0;
        MaxRefrigFlow = 0.0;
        ActRefrigFlow = 0.0;
        SysRunning = true;

        if (GetCurrentScheduleValue(DRink(SysNum).SchedPtr) <= 0) {
            // Unit is off or has no load upon it; set the flow rates to zero and then
            // simulate the components with the no flow conditions

            for (SurfNum = 1; SurfNum <= DRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                    SurfNum2 = DRink(SysNum).SurfacePtrArray(SurfNum);
                    QRadSysSource(SurfNum2) = 0.0;
                    if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                        QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                }
            }

            mdot = 0.0;
            SetComponentFlowRate(mdot,
                                 DRink(SysNum).ColdRefrigInNode,
                                 DRink(SysNum).ColdRefrigOutNode,
                                 DRink(SysNum).CRefrigLoopNum,
                                 DRink(SysNum).CRefrigLoopSide,
                                 DRink(SysNum).CRefrigBranchNum,
                                 DRink(SysNum).CRefrigBranchNum);

        } else { // Unit might be on-->this section is intended to control the refrigerant mass flow rate being
            // sent to the radiant system
            {
                auto const SELECT_CASE_var(DRink(SysNum).ControlType);
                if (SELECT_CASE_var == SurfaceTempControl) {
                    ControlTemp = STC(DirectSystem, SysNum);
                } else if (SELECT_CASE_var == BrineOutletTempControl) {
                    ControlTemp = BOTC(DirectSystem, SysNum);
                } else { // Should never get here
                    ControlTemp = STC(DirectSystem, SysNum);
                    ShowSevereError("Illegal control type in direct refrigeration system: " + DRink(SysNum).Name);
                    ShowFatalError("Preceding condition causes termination.");
                }
            }

            if (DRink(SysNum).ColdSetptSchedPtr > 0) {
                SetPointTemp = GetCurrentScheduleValue(DRink(SysNum).ColdSetptSchedPtr);
                OffTempCool = SetPointTemp - 0.5 * DRink(SysNum).ColdThrottleRange;
            } else { // This system is not capable of cooling, set OffTempCool to something really high
                OffTempCool = HighTempCooling;
            }

            if (ControlTemp > OffTempCool) {
                OperatingMode = CoolingMode;
                ControlNode = DRink(SysNum).ColdRefrigInNode;
                MaxRefrigFlow = DRink(SysNum).RefrigFlowMaxCool;
                MassFlowFrac = (ControlTemp - OffTempCool) / DRink(SysNum).ColdThrottleRange;
            } else {
                MassFlowFrac = 0.0;
            }

            // Calculate and limit the refrigerant flow rate
            ActRefrigFlow = MassFlowFrac * MaxRefrigFlow;
            if (ActRefrigFlow < MassFlowTolerance) ActRefrigFlow = 0.0;
            if (OperatingMode == CoolingMode) {

                SetComponentFlowRate(ActRefrigFlow,
                                     DRink(SysNum).ColdRefrigInNode,
                                     DRink(SysNum).ColdRefrigOutNode,
                                     DRink(SysNum).CRefrigLoopNum,
                                     DRink(SysNum).CRefrigLoopSide,
                                     DRink(SysNum).CRefrigBranchNum,
                                     DRink(SysNum).CRefrigCompNum);

            } else {
                SysRunning = false;
            }

            // Now simulate the system...
            if ((OperatingMode == CoolingMode) && SysRunning) CalcDirectIndoorIceRinkComps(SysNum, LoadMet);
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

    Real64 BOTC(int SystemType, int SysNum)
    {
        // Using/ Aliasing
        using DataGlobals::Pi;
        using DataHeatBalFanSys::CTFTsrcConstPart;
        using DataHeatBalFanSys::RadSysTiHBConstCoef;
        using DataHeatBalFanSys::RadSysTiHBQsrcCoef;
        using DataHeatBalFanSys::RadSysTiHBToutCoef;
        using DataHeatBalFanSys::RadSysToHBConstCoef;
        using DataHeatBalFanSys::RadSysToHBQsrcCoef;
        using DataHeatBalFanSys::RadSysToHBTinCoef;
        using DataLoopNode::Node;
        using DataSurfaces::SurfaceClass_Floor;
        using FluidProperties::GetSatSpecificHeatRefrig;
        using FluidProperties::GetSpecificHeatGlycol;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("BrineOutletTemperatureControl");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum;                    // Index to the surface number having source/sink (Floor)
        int ConstrNum;                  // Index for construction number in Construct derived type
        Real64 Eff;                     // Effectiveness of the heat exchanger
        Real64 RefrigInTemp;            // Refrigerant inlet temperature
        Real64 RefrigOutTemp;           // Refrigerant outlet temperature
        Real64 RefrigOutTempDesired;    // Refrigerant outlet temperature which is desired,
                                        // so that BOTC can be established (to be obtained from the set point temperature)
        int RefrigNodeIn;               // Inlet node of refrigerant
        Real64 RefrigMassFlow;          // Initial guess value for mass flow rate of refrigerant
        Real64 RefrigMassFlow_Req(0.0); // Required mass flow rate of refrigerant to satisfy the BOTC
        Real64 CpRef;                   // Specific heat of Ammonia used in direct refrigeration system
        Real64 QSource;                 // Heat flux from the heat source/sink
        Real64 PipeArea;                // Total area of the pipe used in the ice rink
        Real64 Ca;                      // Coefficients to relate the inlet water temperature to the heat source
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

        // FLOW:

        if (SystemType == DirectSystem) {
            PipeArea = Pi * DRink(SysNum).TubeDiameter * DRink(SysNum).TubeLength;
            RefrigOutTempDesired = DRink(SysNum).RefOutBOTCtrlTemp;
            RefrigMassFlow = 20.0;
            RefrigNodeIn = DRink(SysNum).ColdRefrigInNode;
            RefrigInTemp = Node(RefrigNodeIn).Temp;
            CpRef = GetSatSpecificHeatRefrig(DRink(SysNum).RefrigerantName, RefrigInTemp, 0.0, DRink(SysNum).RefIndex, RoutineName);

            Eff = CalcDRinkHXEffectTerm(RefrigInTemp, SysNum, RefrigMassFlow, DRink(SysNum).TubeLength, DRink(SysNum).TubeDiameter) /
                  (RefrigMassFlow * CpRef);
            for (SurfNum = 1; SurfNum <= DRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(DRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                    ConstrNum = Surface(SurfNum).Construction;

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

                    QSource = (RefrigInTemp - Ck) / ((Cl / Surface(SurfNum).Area) + (1 / (RefrigMassFlow * CpRef)));

                    RefrigOutTemp = RefrigInTemp - (QSource / (RefrigMassFlow * CpRef));

                    if (RefrigOutTemp <= RefrigOutTempDesired) { // Cooling is not required and refrigeration system should be off

                        RefrigOutTemp = RefrigOutTempDesired;
                        RefrigMassFlow = DRink(SysNum).RefrigFlowMinCool;

                    } else if (RefrigOutTemp > RefrigOutTempDesired) { // Cooling is required and refrigeration system should be on

                        RefrigMassFlow = (((Ck - RefrigInTemp) / (RefrigOutTemp - RefrigInTemp)) - (1 / Eff)) * ((PipeArea) / (CpRef * Cl));

                        if (RefrigMassFlow >= DRink(SysNum).RefrigFlowMaxCool) { // The refrigeration system is undersized
                            RefrigMassFlow = DRink(SysNum).RefrigFlowMaxCool;
                        }
                    }
                }
            }
        } else if (SystemType == IndirectSystem) {
            PipeArea = Pi * IRink(SysNum).TubeDiameter * IRink(SysNum).TubeLength;
            RefrigOutTempDesired = IRink(SysNum).RefOutBOTCtrlTemp;
            RefrigMassFlow = 0.1;
            RefrigNodeIn = IRink(SysNum).ColdRefrigInNode;
            RefrigInTemp = Node(RefrigNodeIn).Temp;
            CpRef = GetSpecificHeatGlycol(IRink(SysNum).RefrigerantName, RefrigInTemp, IRink(SysNum).RefIndex, RoutineName);

            Eff = CalcIRinkHXEffectTerm(RefrigInTemp,
                                        SysNum,
                                        RefrigMassFlow,
                                        IRink(SysNum).TubeLength,
                                        IRink(SysNum).TubeDiameter,
                                        IRink(SysNum).RefrigType,
                                        IRink(SysNum).Concentration) /
                  (RefrigMassFlow * CpRef);

            for (SurfNum = 1; SurfNum <= IRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(IRink(SysNum).SurfacePtrArray(SurfNum)).Class == SurfaceClass_Floor) {
                    ConstrNum = Surface(SurfNum).Construction;

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

                    QSource = (RefrigInTemp - Ck) / ((Cl / Surface(SurfNum).Area) + (1 / (RefrigMassFlow * CpRef)));

                    RefrigOutTemp = RefrigInTemp - (QSource / (RefrigMassFlow * CpRef));

                    if (RefrigOutTemp <= RefrigOutTempDesired) { // Cooling is not required and refrigeration system should be off

                        RefrigOutTemp = RefrigOutTempDesired;
                        RefrigMassFlow_Req = IRink(SysNum).RefrigFlowMinCool;

                    } else if (RefrigOutTemp > RefrigOutTempDesired) { // Cooling is required and refrigeration system should be on

                        RefrigMassFlow_Req = (((Ck - RefrigInTemp) / (RefrigOutTemp - RefrigInTemp)) - (1 / Eff)) * ((PipeArea) / (CpRef * Cl));

                        if (RefrigMassFlow >= IRink(SysNum).RefrigFlowMaxCool) { // The refrigeration system is undersized
                            RefrigMassFlow = IRink(SysNum).RefrigFlowMaxCool;
                        }
                    }
                }
            }
        }

        return RefrigMassFlow_Req;
    }

    Real64 STC(int const SysTemType, int const SysNum)
    {
        return (0.0);
    }

} // namespace IceRink
} // namespace EnergyPlus
