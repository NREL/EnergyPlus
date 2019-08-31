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

    static std::string const fluidNameWater("WATER");
    static std::string const fluidNameBrine("BRINE");
    static std::string const fluidNameAmmonia("NH3");

    // MODULE VARIABLE DECLARATIONS:
    bool GetInputFlag = true;
    int NumIceRinks(0); // Number of Ice Rinks
    Array1D_bool CheckEquipName;
    int NumOfDirectSystem(0);             // Number of direct refrigeration systems
    int NumOfIndirectSystem(0);           // Number of direct refrigeration systems
    int TotalNumRefrigSystem(0);          // Total number of refrigeration systems
    int OperatingMode(0);                 // Used to keep track of whether system is in heating or cooling mode
    int NotOperating(0);                  // Parameter for use with OperatingMode variable, set for not operating
    int CoolingMode(2);                   // Parameter for use with OperatingMode variable, set for cooling
    bool FirstTimeInit(true);             // Set to true for first pass through init routine then set to false
    Array1D<Real64> QRadSysSrcAvg;        // Average source over the time step for a particular radiant surface
    Array1D<Real64> ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
    // Record keeping variables used to calculate QRadSysSrcAvg locally
    Array1D<Real64> LastQRadSysSrc;     // Need to keep the last value in case we are still iterating
    Array1D<Real64> LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
    Array1D<Real64> LastTimeStepSys;    // Need to keep the last value in case we are still iterating
    // Autosizing variables
    Array1D_bool MySizeFlagDirSys;
    Array1D_bool MySizeFlagIndirSys;

    // Object Data
    Array1D<DirectRefrigSysData> DRink;
    Array1D<IndirectRefrigSysData> IRink;
    Array1D<RefrigSysTypeData> RefrigSysTypes;
    Array1D<ResurfacerData> Resurfacer;
    /*Array1D<DirectRefrigSysNumericFieldData> DirectRefrigSysNumericFields;
    Array1D<IndirectRefrigSysNumericFieldData> IndirectRefrigSysNumericFields;
    std::unordered_map<std::string, std::string> RinkRefrigUniqueNames;*/

    // Functions:

    void InitIndoorIceRink(bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                           bool &InitErrorsFound,         // TRUE if some error is found in the initialization
                           int const SystemType,          // Type of refrigeration system: Direct or Indirect
                           int const SysNum,              // Index to the refrigeration system
                           int const ResurfacerIndex      // Index to the resurfacer
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Punya Sloka Dash
        //       DATE WRITTEN   August 2019

        // Using/Aliasing
        using DataGlobals::AnyPlantInModel;
        using DataGlobals::BeginEnvrnFlag;
        using DataGlobals::SysSizingCalc;
        using DataPlant::PlantLoop;
        using DataPlant::TypeOf_LowTempRadiant_VarFlow;
        using namespace DataSurfaces;
        using DataSurfaces::SurfaceClass_Floor;
        using FluidProperties::GetDensityGlycol;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitIndoorIceRink");
        Real64 const MinActivityFactor = 0.0;  // Minimum value for activity factor
        Real64 const MaxActivityFactor = 10.0; // Maximum value for activity factor (realistically)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int SystemNum;                   // Number of the refrigerant system (Do loop counter)
        int SurfNum;                     // Intermediate variable for keeping track of the surface number
        int SurfNum1;                    // Intermediate variable for keeping track of the surface number
        int ZoneNum;                     // Intermediate variable for keeping track of the zone number
        static bool MyOneTimeFlag(true); // Initialization flag
        static bool MyEnvrnFlagGeneral(true);
        Real64 mdot; // local fluid mass flow rate
        Real64 rho;  // local fluid density
        bool errFlag;
        Real64 HeatGainPerPerson;
        Real64 PeopleModifier;
        static Array1D_bool MyEnvrnFlagDRink;
        static Array1D_bool MyEnvrnFlagIRink;
        static Array1D_bool MyPlantScanFlagDRink;
        static Array1D_bool MyPlantScanFlagIRink;

        InitErrorsFound = false;

        if (MyOneTimeFlag) {
            MyEnvrnFlagDRink.allocate(NumOfDirectSystem);
            MyEnvrnFlagIRink.allocate(NumOfIndirectSystem);
            MyPlantScanFlagDRink.allocate(NumOfDirectSystem);
            MyPlantScanFlagIRink.allocate(NumOfIndirectSystem);
            MyEnvrnFlagDRink = true;
            MyEnvrnFlagIRink = true;
            MyPlantScanFlagDRink = true;
            MyPlantScanFlagIRink = true;
            MyOneTimeFlag = false;
        }

        if (FirstTimeInit) {
            QRadSysSrcAvg.dimension(TotSurfaces, 0.0);
            LastQRadSysSrc.dimension(TotSurfaces, 0.0);
            LastSysTimeElapsed.dimension(TotSurfaces, 0.0);
            LastTimeStepSys.dimension(TotSurfaces, 0.0);
            MySizeFlagDirSys.allocate(NumOfDirectSystem);
            MySizeFlagIndirSys.allocate(NumOfIndirectSystem);
            MySizeFlagDirSys = true;
            MySizeFlagIndirSys = true;

            // Initialize total areas for the radiant system
            for (SystemNum = 1; SystemNum <= NumOfDirectSystem; ++SystemNum) {
                DRink(SystemNum).TotalSurfaceArea = 0.0;
                for (SurfNum = 1; SurfNum <= DRink(SystemNum).NumOfSurfaces; ++SurfNum) {
                    if (Surface(DRink(SysNum).SurfacePtr).Class == SurfaceClass_Floor) {
                        DRink(SystemNum).TotalSurfaceArea += DRink(SystemNum).TotalSurfaceArea;
                    }
                }
            }

            for (SystemNum = 1; SystemNum <= NumOfIndirectSystem; ++SystemNum) {
                IRink(SystemNum).TotalSurfaceArea = 0.0;
                for (SurfNum = 1; SurfNum <= IRink(SystemNum).NumOfSurfaces; ++SurfNum) {
                    if (Surface(IRink(SysNum).SurfacePtr).Class == SurfaceClass_Floor) {
                        IRink(SystemNum).TotalSurfaceArea += IRink(SystemNum).TotalSurfaceArea;
                    }
                }
            }

            FirstTimeInit = false;
        }
        if (SystemType == DirectSystem) {
            if (MyPlantScanFlagDRink(SysNum) && allocated(PlantLoop)) {
                errFlag = false;
                if (DRink(SysNum).ColdRefrigInNode > 0) {
                    ScanPlantLoopsForObject(DRink(SysNum).Name,
                                            TypeOf_LowTempRadiant_VarFlow,
                                            DRink(SysNum).CRefrigLoopNum,
                                            DRink(SysNum).CRefrigLoopSide,
                                            DRink(SysNum).CRefrigBranchNum,
                                            DRink(SysNum).CRefrigCompNum,
                                            _,
                                            _,
                                            _,
                                            DRink(SysNum).ColdRefrigInNode,
                                            _,
                                            errFlag);
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
                if (IRink(SysNum).ColdBrineInNode > 0) {
                    ScanPlantLoopsForObject(IRink(SysNum).Name,
                                            TypeOf_LowTempRadiant_VarFlow,
                                            IRink(SysNum).CBLoopNum,
                                            IRink(SysNum).CBLoopSide,
                                            IRink(SysNum).CBBranchNum,
                                            IRink(SysNum).CBCompNum,
                                            _,
                                            _,
                                            _,
                                            IRink(SysNum).ColdBrineInNode,
                                            _,
                                            errFlag);
                    if (errFlag) {
                        ShowFatalError("InitIndoorIceRink: Program terminated due to previous condition(s).");
                    }
                }
                MyPlantScanFlagIRink(SysNum) = false;
            } else if (MyPlantScanFlagIRink(SysNum) && !AnyPlantInModel) {
                MyPlantScanFlagIRink(SysNum) = false;
            }
        }

        if (!SysSizingCalc && (SystemType == DirectSystem)) {
            if (MySizeFlagDirSys(SysNum) && !MyPlantScanFlagDRink(SysNum)) {
                // for all direct refrigerant systems do the sizing once.
                SizeRefrigSysTubeLength(SysNum, SystemType);
                MySizeFlagDirSys = false;

                // Can this system actually do cooling?
                if ((DRink(SysNum).RefrigVolFlowMaxCool > 0.0) && (DRink(SysNum).ColdRefrigInNode > 0) && (DRink(SysNum).ColdRefrigOutNode > 0) &&
                    (DRink(SysNum).ColdSetptSchedPtr > 0)) {
                    DRink(SysNum).CoolingSystem = true;
                }

                // set design mass flow rates
                if (DRink(SysNum).ColdRefrigInNode > 0) {
                    rho = GetDensityGlycol(PlantLoop(DRink(SysNum).CRefrigLoopNum).FluidName,
                                           DataGlobals::CWInitConvTemp,
                                           PlantLoop(DRink(SysNum).CRefrigLoopNum).FluidIndex,
                                           RoutineName);
                    DRink(SysNum).RefrigFlowMaxCool = rho * DRink(SysNum).RefrigVolFlowMaxCool;
                    InitComponentNodes(0.0,
                                       DRink(SysNum).RefrigFlowMaxCool,
                                       DRink(SysNum).ColdRefrigInNode,
                                       DRink(SysNum).ColdRefrigOutNode,
                                       DRink(SysNum).CRefrigLoopNum,
                                       DRink(SysNum).CRefrigLoopSide,
                                       DRink(SysNum).CRefrigBranchNum,
                                       DRink(SysNum).CRefrigCompNum);
                }
            }
        }

        if (!SysSizingCalc && (SystemType == IndirectSystem)) {
            if (MySizeFlagIndirSys(SysNum) && !MyPlantScanFlagIRink(SysNum)) {
                // for all direct refrigerant systems do the sizing once.
                SizeRefrigSysTubeLength(SysNum, SystemType);
                MySizeFlagIndirSys = false;

                // Can this system actually do cooling?
                if ((IRink(SysNum).RefrigVolFlowMaxCool > 0.0) && (IRink(SysNum).ColdBrineInNode > 0) && (IRink(SysNum).ColdBrineOutNode > 0) &&
                    (IRink(SysNum).ColdSetptSchedPtr > 0)) {
                    IRink(SysNum).CoolingSystem = true;
                }

                // set design mass flow rates
                if (IRink(SysNum).ColdBrineInNode > 0) {
                    rho = GetDensityGlycol(PlantLoop(IRink(SysNum).CBLoopNum).FluidName,
                                           DataGlobals::CWInitConvTemp,
                                           PlantLoop(IRink(SysNum).CBLoopNum).FluidIndex,
                                           RoutineName);
                    IRink(SysNum).BrineFlowMaxCool = rho * IRink(SysNum).RefrigVolFlowMaxCool;
                    InitComponentNodes(0.0,
                                       IRink(SysNum).BrineFlowMaxCool,
                                       IRink(SysNum).ColdBrineInNode,
                                       IRink(SysNum).ColdBrineOutNode,
                                       IRink(SysNum).CBLoopNum,
                                       IRink(SysNum).CBLoopSide,
                                       IRink(SysNum).CBBranchNum,
                                       IRink(SysNum).CBCompNum);
                }
            }
        }

        if (BeginEnvrnFlag && MyEnvrnFlagGeneral) {
            QRadSysSrcAvg = 0.0;
            LastQRadSysSrc = 0.0;
            LastSysTimeElapsed = 0.0;
            LastTimeStepSys = 0.0;
            MyEnvrnFlagGeneral = false;
        }
        if (!BeginEnvrnFlag) MyEnvrnFlagGeneral = true;

        if (SystemType == DirectSystem) {
            if (BeginEnvrnFlag && MyEnvrnFlagDRink(SysNum)) {
                DRink(SysNum).CoolPower = 0.0;
                DRink(SysNum).CoolEnergy = 0.0;
                DRink(SysNum).RefrigMassFlowRate = 0.0;
                DRink(SysNum).RefrigInletTemp = 0.0;
                DRink(SysNum).RefrigOutletTemp = 0.0;
                DRink(SysNum).MiscEquipPower = 0.0;
                DRink(SysNum).MiscEquipEnergy = 0.0;
                Resurfacer(ResurfacerIndex).QResurfacing = 0.0;
                Resurfacer(ResurfacerIndex).EHeatingWater = 0.0;
                Resurfacer(ResurfacerIndex).QHumidity = 0.0;

                if (!MyPlantScanFlagDRink(SysNum)) {
                    if (DRink(SysNum).ColdRefrigInNode > 0) {
                        InitComponentNodes(0.0,
                                           DRink(SysNum).RefrigFlowMaxCool,
                                           DRink(SysNum).ColdRefrigInNode,
                                           DRink(SysNum).ColdRefrigOutNode,
                                           DRink(SysNum).CRefrigLoopNum,
                                           DRink(SysNum).CRefrigLoopSide,
                                           DRink(SysNum).CRefrigBranchNum,
                                           DRink(SysNum).CRefrigCompNum);
                    }
                }
                MyEnvrnFlagDRink(SysNum) = false;
            }
        }
        if (!BeginEnvrnFlag && SystemType == DirectSystem) MyEnvrnFlagDRink(SysNum) = true;

        if (SystemType == IndirectSystem) {
            if (BeginEnvrnFlag && MyEnvrnFlagIRink(SysNum)) {
                IRink(SysNum).CoolPower = 0.0;
                IRink(SysNum).CoolEnergy = 0.0;
                IRink(SysNum).BrineInletTemp = 0.0;
                IRink(SysNum).BrineOutletTemp = 0.0;
                IRink(SysNum).BrineMassFlowRate = 0.0;
                IRink(SysNum).MiscEquipPower = 0.0;
                IRink(SysNum).MiscEquipEnergy = 0.0;
                Resurfacer(ResurfacerIndex).QResurfacing = 0.0;
                Resurfacer(ResurfacerIndex).EHeatingWater = 0.0;
                Resurfacer(ResurfacerIndex).QHumidity = 0.0;

                if (!MyPlantScanFlagIRink(SysNum)) {
                    if (IRink(SysNum).ColdBrineInNode > 0) {
                        InitComponentNodes(0.0,
                                           IRink(SysNum).BrineFlowMaxCool,
                                           IRink(SysNum).ColdBrineInNode,
                                           IRink(SysNum).ColdBrineOutNode,
                                           IRink(SysNum).CBLoopNum,
                                           IRink(SysNum).CBLoopSide,
                                           IRink(SysNum).CBBranchNum,
                                           IRink(SysNum).CBCompNum);
                    }
                }
                MyEnvrnFlagIRink(SysNum) = false;
            }
        }
        if (!BeginEnvrnFlag && SystemType == IndirectSystem) MyEnvrnFlagIRink(SysNum) = true;

        if (BeginTimeStepFlag && FirstHVACIteration) { // This is the first pass through in a particular time step

            {
                auto const SELECT_CASE_var(SystemType);

                if (SELECT_CASE_var == DirectSystem) {
                    ZoneNum = DRink(SysNum).ZonePtr;
                    ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum); // Set this to figure what part of the load the radiant system meets
                    for (SurfNum = 1; SurfNum <= DRink(SysNum).NumOfSurfaces; ++SurfNum) {
                        SurfNum1 = DRink(SysNum).SurfacePtr2(SurfNum);
                        QRadSysSrcAvg(SurfNum) = 0.0;      // Initialize this variable to zero (radiant system defaults to off)
                        LastQRadSysSrc(SurfNum) = 0.0;     // At the start of a time step, reset to zero so average calculation can begin again
                        LastSysTimeElapsed(SurfNum) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
                        LastTimeStepSys(SurfNum) = 0.0;    // At the start of a time step, reset to zero so average calculation can begin again
                    }

                } else if (SELECT_CASE_var == IndirectSystem) {
                    ZoneNum = IRink(SysNum).ZonePtr;
                    ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum); // Set this to figure what part of the load the radiant system meets
                    for (SurfNum = 1; SurfNum <= IRink(SysNum).NumOfSurfaces; ++SurfNum) {
                        SurfNum1 = IRink(SysNum).SurfacePtr2(SurfNum);
                        QRadSysSrcAvg(SurfNum) = 0.0;      // Initialize this variable to zero (radiant system defaults to off)
                        LastQRadSysSrc(SurfNum) = 0.0;     // At the start of a time step, reset to zero so average calculation can begin again
                        LastSysTimeElapsed(SurfNum) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
                        LastTimeStepSys(SurfNum) = 0.0;    // At the start of a time step, reset to zero so average calculation can begin again
                    }
                } else {

                    ShowSevereError("Refrigeration system entered without specification of type: Direct of Indirect?");
                    ShowContinueError("Occurs in Refrigeration System =" + DRink(SysNum).Name);
                    ShowFatalError("Preceding condition causes termination.");
                }
            }
        }

        {
            auto const SELECT_CASE_var(SystemType);

            if (SELECT_CASE_var == DirectSystem) {

                // Initiate the appropriate node data
                if (DRink(SysNum).CoolingSystem) {
                    mdot = 0.0;
                    SetComponentFlowRate(mdot,
                                         DRink(SysNum).ColdRefrigInNode,
                                         DRink(SysNum).ColdRefrigOutNode,
                                         DRink(SysNum).CRefrigLoopNum,
                                         DRink(SysNum).CRefrigLoopSide,
                                         DRink(SysNum).CRefrigBranchNum,
                                         DRink(SysNum).CRefrigCompNum);
                }
            } else if (SELECT_CASE_var == IndirectSystem) {
                if (IRink(SysNum).CoolingSystem) {
                    mdot = 0.0;
                    SetComponentFlowRate(mdot,
                                         IRink(SysNum).ColdBrineInNode,
                                         IRink(SysNum).ColdBrineOutNode,
                                         IRink(SysNum).CBLoopNum,
                                         IRink(SysNum).CBLoopSide,
                                         IRink(SysNum).CBBranchNum,
                                         IRink(SysNum).CBCompNum);
                }
            }
        }

        // get the schedule values for different scheduled parameters

        {
            auto const SELECT_CASE_var(SystemType);

            if (SystemType == DirectSystem) {
                if (DRink(SysNum).ActivityFactorSchedPtr > 0) {
                    DRink(SysNum).CurActivityFactor = GetCurrentScheduleValue(DRink(SysNum).ActivityFactorSchedPtr);
                    if (DRink(SysNum).CurActivityFactor < MinActivityFactor) {
                        DRink(SysNum).CurActivityFactor = MinActivityFactor;
                        ShowWarningError(RoutineName + ": Indoor Ice Rink =\"" + DRink(SysNum).Name + " Activity Factor Schedule =\"" +
                                         DRink(SysNum).ActivityFactorSchedName + " has a negative value.  This is not allowed.");
                        ShowContinueError("The activity factor has been reset to zero.");
                    }
                    if (DRink(SysNum).CurActivityFactor > MaxActivityFactor) {
                        DRink(SysNum).CurActivityFactor = 1.0;
                        ShowWarningError(RoutineName + ": Indoor Ice Rink =\"" + DRink(SysNum).Name + " Activity Factor Schedule =\"" +
                                         DRink(SysNum).ActivityFactorSchedName + " has a value larger than 10.  This is not allowed.");
                        ShowContinueError("The activity factor has been reset to unity.");
                    }
                } else {
                    // default is activity factor of 1.0
                    DRink(SysNum).CurActivityFactor = 1.0;
                }
            } else if (SystemType == IndirectSystem) {
                if (IRink(SysNum).ActivityFactorSchedPtr > 0) {
                    IRink(SysNum).CurActivityFactor = GetCurrentScheduleValue(IRink(SysNum).ActivityFactorSchedPtr);
                    if (IRink(SysNum).CurActivityFactor < MinActivityFactor) {
                        IRink(SysNum).CurActivityFactor = MinActivityFactor;
                        ShowWarningError(RoutineName + ": Indoor Ice Rink =\"" + IRink(SysNum).Name + " Activity Factor Schedule =\"" +
                                         IRink(SysNum).ActivityFactorSchedName + " has a negative value.  This is not allowed.");
                        ShowContinueError("The activity factor has been reset to zero.");
                    }
                    if (IRink(SysNum).CurActivityFactor > MaxActivityFactor) {
                        IRink(SysNum).CurActivityFactor = 1.0;
                        ShowWarningError(RoutineName + ": Indoor Ice Rink =\"" + IRink(SysNum).Name + " Activity Factor Schedule =\"" +
                                         IRink(SysNum).ActivityFactorSchedName + " has a value larger than 10.  This is not allowed.");
                        ShowContinueError("The activity factor has been reset to unity.");
                    }
                } else {
                    // default is activity factor of 1.0
                    IRink(SysNum).CurActivityFactor = 1.0;
                }
            }

            if (SystemType == DirectSystem)
                DRink(SysNum).CurSetPtTemp = GetCurrentScheduleValue(DRink(SysNum).SetPtTempSchedPtr);
            else if (SystemType == IndirectSystem)
                IRink(SysNum).CurSetPtTemp = GetCurrentScheduleValue(IRink(SysNum).SetPtTempSchedPtr);

            if (Resurfacer(ResurfacerIndex).ResurfacingSchedPtr > 0)
                Resurfacer(ResurfacerIndex).ResurfacingWaterTemp = GetCurrentScheduleValue(Resurfacer(ResurfacerIndex).ResurfacingSchedPtr);
            else {
                // set resurfacing water temperature to be 15 C
                Resurfacer(ResurfacerIndex).ResurfacingWaterTemp = 15.0;
            }

            // Determine the heat gain from people
            if (SystemType == DirectSystem) {
                if (DRink(SysNum).PeopleHeatGainSchedPtr > 0) {
                    HeatGainPerPerson = GetCurrentScheduleValue(DRink(SysNum).PeopleHeatGainSchedPtr);
                    if (HeatGainPerPerson < 0.0) {
                        ShowWarningError(RoutineName + ": Indoor Ice Rink =\"" + DRink(SysNum).Name + " Heat Gain Schedule =\"" +
                                         DRink(SysNum).PeopleHeatGainSchedName + " has a negative value.  This is not allowed.");
                        ShowContinueError("The heat gain per person has been reset to zero.");
                        HeatGainPerPerson = 0.0;
                    }
                    if (DRink(SysNum).PeopleHeatGainSchedPtr > 0) {
                        PeopleModifier = GetCurrentScheduleValue(DRink(SysNum).PeopleSchedPtr);
                        if (PeopleModifier < 0.0) {
                            ShowWarningError(RoutineName + ": Indoor Ice Rink =\"" + DRink(SysNum).Name + " People Schedule =\"" +
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
            }
            if (SystemType == IndirectSystem) {
                if (IRink(SysNum).PeopleHeatGainSchedPtr > 0) {
                    HeatGainPerPerson = GetCurrentScheduleValue(IRink(SysNum).PeopleHeatGainSchedPtr);
                    if (HeatGainPerPerson < 0.0) {
                        ShowWarningError(RoutineName + ": Indoor Ice Rink =\"" + IRink(SysNum).Name + " Heat Gain Schedule =\"" +
                                         IRink(SysNum).PeopleHeatGainSchedName + " has a negative value.  This is not allowed.");
                        ShowContinueError("The heat gain per person has been reset to zero.");
                        HeatGainPerPerson = 0.0;
                    }
                    if (IRink(SysNum).PeopleHeatGainSchedPtr > 0) {
                        PeopleModifier = GetCurrentScheduleValue(IRink(SysNum).PeopleSchedPtr);
                        if (PeopleModifier < 0.0) {
                            ShowWarningError(RoutineName + ": Indoor Ice Rink =\"" + IRink(SysNum).Name + " People Schedule =\"" +
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

        OperatingMode = NotOperating;
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
        Real64 DeltaHWaterToIce;
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
        return EHeatingWater;
        return QHumidity;
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
                if (Surface(DRink(SysNum).SurfacePtr2(SurfNum)).Class == SurfaceClass_Floor) {
                    SurfNum2 = DRink(SysNum).SurfacePtr2(SurfNum);
                    QRadSysSource(SurfNum2) = 0.0;
                    if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                        QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                }
            }
            if (DRink(SysNum).CoolingSystem) {
                mdot = 0.0;
                SetComponentFlowRate(mdot,
                                     DRink(SysNum).ColdRefrigInNode,
                                     DRink(SysNum).ColdRefrigOutNode,
                                     DRink(SysNum).CRefrigLoopNum,
                                     DRink(SysNum).CRefrigLoopSide,
                                     DRink(SysNum).CRefrigBranchNum,
                                     DRink(SysNum).CRefrigBranchNum);
            }

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
                OffTempCool = SetPointTemp - 0.5 * DRink(SysNum).ColdThrottlRange;
            } else { // This system is not capable of cooling, set OffTempCool to something really high
                OffTempCool = HighTempCooling;
            }

            if (ControlTemp > OffTempCool && DRink(SysNum).CoolingSystem) {
                OperatingMode = CoolingMode;
                ControlNode = DRink(SysNum).ColdRefrigInNode;
                MaxRefrigFlow = DRink(SysNum).RefrigFlowMaxCool;
                MassFlowFrac = (ControlTemp - OffTempCool) / DRink(SysNum).ColdThrottlRange;
            } else {
                MassFlowFrac = 0.0;
            }

            // Calculate and limit the refrigerant flow rate
            ActRefrigFlow = MassFlowFrac * MaxRefrigFlow;
            if (ActRefrigFlow < MassFlowTolerance) ActRefrigFlow = 0.0;
            if (OperatingMode == CoolingMode) {
                if (DRink(SysNum).CoolingSystem) {
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
            }

            // Now simulate the system...
            if ((OperatingMode == CoolingMode) && SysRunning) CalcDirectIndoorIceRinkComps(SysNum, LoadMet);
        }
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
        int RefrigNodeIn;          // Node number of the refrigerant entering the refrigerant system
        int ZoneNum;               // Zone pointer for this refrigerant system
        int SurfNum;               // DO loop counter for the surfaces that comprise a particular radiant system
        int SurfNum2;              // Index to the floor in the surface derived type
        int SurfNum3;              // DO loop counter for the surfaces that comprise a particular radiant system
        int SurfNumC;              // Index to the surfaces used for condensation control
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
                if (Surface(DRink(SysNum).SurfacePtr).Class == SurfaceClass_Floor) {
                    SurfNum2 = DRink(SysNum).SurfacePtr2(SurfNum);
                    QRadSysSource(SurfNum2) = 0.0;
                    if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                        QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                }
            }
        } else {
            for (SurfNum = 1; SurfNum <= DRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(DRink(SysNum).SurfacePtr).Class == SurfaceClass_Floor) {
                    // Determine the heat exchanger "effectiveness" term
                    EpsMdotCp = CalcDRinkHXEffectTerm(RefrigTempIn, SysNum, RefrigMassFlow, DRink(SysNum).TubeLength, DRink(SysNum).TubeLength);

                    ConstrNum = Surface(SurfNum).Construction;
                    if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CTF) {

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

            // "Temperature Comparision" Cut-off:
            for (SurfNum = 1; SurfNum <= DRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(DRink(SysNum).SurfacePtr).Class == SurfaceClass_Floor) {
                    // Check to see whether or not the system should really be running.  If
                    // QRadSysSource is positive when we are in cooling mode, then the radiant system
                    // will be doing the opposite of its intention.  In this case, the flow rate
                    // is set to zero to avoid heating in cooling mode
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

                    for (SurfNum2 = 1; SurfNum2 <= DRink(SysNum).NumOfSurfaces; ++SurfNum2) {
                        SurfNum2 = DRink(SysNum).SurfacePtr2(SurfNum2);
                        QRadSysSource(SurfNum2) = 0.0;
                        if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                            QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                    }
                    break;
                }
            }

            // Condensation Cut-off:
            // Check to see whether there are any surface temperatures within the radiant system that have
            // dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
            // A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
            // conditions.
            DRink(SysNum).CondCausedShutDown = false;
            DewPointTemp = PsyTdpFnWPb(ZoneAirHumRat(ZoneNum), OutBaroPress);

            if ((OperatingMode == CoolingMode) && (DRink(SysNum).CondCtrlType == CondCtrlSimpleOff)) {
                for (SurfNumC = 1; SurfNumC <= DRink(SysNum).NumOfSurfaces; ++SurfNumC) {
                    if (TH(2, 1, DRink(SysNum).SurfacePtr2(SurfNumC)) < (DewPointTemp + DRink(SysNum).CondDewPtDeltaT)) {
                        // Condensation warning--must shut off radiant system
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

                        // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                        if (!WarmupFlag) {
                            if (DRink(SysNum).CondErrIndex == 0) {
                                ShowWarningMessage(cDRink + " [" + DRink(SysNum).Name + ']');
                                ShowContinueError("Surface [" + Surface(DRink(SysNum).SurfacePtr2(SurfNumC)).Name +
                                                  "] temperature below dew-point temperature--potential for condensation exists");
                                ShowContinueError("Flow to the radiant system will be shut-off to avoid condensation");
                                ShowContinueError("Predicted radiant system surface temperature = " +
                                                  RoundSigDigits(TH(2, 1, DRink(SysNum).SurfacePtr2(SurfNumC)), 2));
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
            } else if ((OperatingMode == CoolingMode) && (DRink(SysNum).CondCtrlType == CondCtrlNone)) {
                for (SurfNumC = 1; SurfNumC <= DRink(SysNum).NumOfSurfaces; ++SurfNumC) {
                    if (TH(2, 1, DRink(SysNum).SurfacePtr2(SurfNumC)) < DewPointTemp) {
                        // Condensation occuring but user does not want to shut down the system off ever
                        DRink(SysNum).CondCausedShutDown = true;
                    }
                }
            } else if ((OperatingMode == CoolingMode) && (DRink(SysNum).CondCtrlType == CondCtrlVariedOff)) {
                LowestRadSurfTemp = 999.9;
                CondSurfNum = 0;
                for (SurfNumC = 1; SurfNumC <= DRink(SysNum).NumOfSurfaces; ++SurfNumC) {
                    if (TH(2, 1, DRink(SysNum).SurfacePtr2(SurfNumC)) < (DewPointTemp + DRink(SysNum).CondDewPtDeltaT)) {
                        if (TH(2, 1, DRink(SysNum).SurfacePtr2(SurfNumC)) < LowestRadSurfTemp) {
                            LowestRadSurfTemp = TH(2, 1, DRink(SysNum).SurfacePtr2(SurfNumC));
                            CondSurfNum = SurfNumC;
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
                    for (SurfNum3 = 1; SurfNum3 <= DRink(SysNum).NumOfSurfaces; ++SurfNum3) {
                        SurfNum2 = DRink(SysNum).SurfacePtr2(SurfNum3);
                        QRadSysSource(SurfNum2) = 0.0;
                        if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                            QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                    }
                    // Redo the heat balances since we have changed the heat source (set it to zero)
                    HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(ZoneNum);
                    HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(ZoneNum);
                    // Now check all of the surface temperatures.  If any potentially have condensation, leave the system off.
                    for (SurfNum2 = 1; SurfNum2 <= DRink(SysNum).NumOfSurfaces; ++SurfNum2) {
                        if (TH(2, 1, DRink(SysNum).SurfacePtr2(SurfNum2)) < (DewPointTemp + DRink(SysNum).CondDewPtDeltaT)) {
                            DRink(SysNum).CondCausedShutDown = true;
                        }
                    }
                    // If the system does not need to be shut down, then let's see if we can vary the flow based
                    // on the lowest temperature surface from before.  This will use interpolation to try a new
                    // flow rate.
                    if (!DRink(SysNum).CondCausedShutDown) {
                        PredictedCondTemp = DewPointTemp + DRink(SysNum).CondDewPtDeltaT;
                        ZeroFlowSurfTemp = TH(2, 1, DRink(SysNum).SurfacePtr2(CondSurfNum));
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
                        // Go through all of the surfaces again with the new flow rate...
                        for (SurfNum3 = 1; SurfNum3 <= DRink(SysNum).NumOfSurfaces; ++SurfNum3) {
                            SurfNum = DRink(SysNum).SurfacePtr2(SurfNum3);
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
                        // Redo the heat balances since we have changed the heat source
                        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(ZoneNum);
                        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(ZoneNum);

                        // Check for condensation one more time.  If no condensation, we are done.  If there is
                        // condensation, shut things down and be done.
                        for (SurfNum2 = 1; SurfNum2 <= DRink(SysNum).NumOfSurfaces; ++SurfNum2) {
                            if (DRink(SysNum).CondCausedShutDown) break;
                            if (TH(2, 1, DRink(SysNum).SurfacePtr2(SurfNum2)) < (PredictedCondTemp)) {
                                // Condensation still present--must shut off radiant system
                                DRink(SysNum).CondCausedShutDown = true;
                                RefrigMassFlow = 0.0;
                                SurfNum = SurfNum2;
                                SetComponentFlowRate(SysRefrigMassFlow,
                                                     DRink(SysNum).ColdRefrigInNode,
                                                     DRink(SysNum).ColdRefrigOutNode,
                                                     DRink(SysNum).CRefrigLoopNum,
                                                     DRink(SysNum).CRefrigLoopSide,
                                                     DRink(SysNum).CRefrigBranchNum,
                                                     DRink(SysNum).CRefrigCompNum);
                                DRink(SysNum).RefrigMassFlowRate = RefrigMassFlow;
                                for (SurfNum3 = 1; SurfNum3 <= DRink(SysNum).NumOfSurfaces; ++SurfNum3) {
                                    SurfNum2 = DRink(SysNum).SurfacePtr2(SurfNum3);
                                    QRadSysSource(SurfNum2) = 0.0;
                                    if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                                        QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                                }
                            }
                        }
                    }

                    if (DRink(SysNum).CondCausedShutDown) {
                        // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                        if (!WarmupFlag) {
                            if (DRink(SysNum).CondErrIndex == 0) {
                                ShowWarningMessage(cDRink + " [" + DRink(SysNum).Name + ']');
                                ShowContinueError("Surface [" + Surface(DRink(SysNum).SurfacePtr2(CondSurfNum)).Name +
                                                  "] temperature below dew-point temperature--potential for condensation exists");
                                ShowContinueError("Flow to the radiant system will be shut-off to avoid condensation");
                                ShowContinueError("Predicted radiant system surface temperature = " +
                                                  RoundSigDigits(TH(2, 1, DRink(SysNum).SurfacePtr2(CondSurfNum)), 2));
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

    void CalcIndirectIndoorIceRinkSys(int const SysNum, // name of the indirect refrigeration system
                                      Real64 &LoadMet   // load met by the indirect refrigeration system, in Watts
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

        if (GetCurrentScheduleValue(IRink(SysNum).SchedPtr) <= 0) {
            // Unit is off or has no load upon it; set the flow rates to zero and then
            // simulate the components with the no flow conditions

            for (SurfNum = 1; SurfNum <= IRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(IRink(SysNum).SurfacePtr2(SurfNum)).Class == SurfaceClass_Floor) {
                    SurfNum2 = IRink(SysNum).SurfacePtr2(SurfNum);
                    QRadSysSource(SurfNum2) = 0.0;
                    if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                        QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                }
            }
            if (IRink(SysNum).CoolingSystem) {
                mdot = 0.0;
                SetComponentFlowRate(mdot,
                                     IRink(SysNum).ColdBrineInNode,
                                     IRink(SysNum).ColdBrineOutNode,
                                     IRink(SysNum).CBLoopNum,
                                     IRink(SysNum).CBLoopSide,
                                     IRink(SysNum).CBBranchNum,
                                     IRink(SysNum).CBBranchNum);
            }

        } else { // Unit might be on-->this section is intended to control the refrigerant mass flow rate being
            // sent to the radiant system
            {
                auto const SELECT_CASE_var(IRink(SysNum).ControlType);
                if (SELECT_CASE_var == SurfaceTempControl) {
                    ControlTemp = STC(DirectSystem, SysNum);
                } else if (SELECT_CASE_var == BrineOutletTempControl) {
                    ControlTemp = BOTC(DirectSystem, SysNum);
                } else { // Should never get here
                    ControlTemp = STC(DirectSystem, SysNum);
                    ShowSevereError("Illegal control type in direct refrigeration system: " + IRink(SysNum).Name);
                    ShowFatalError("Preceding condition causes termination.");
                }
            }

            if (IRink(SysNum).ColdSetptSchedPtr > 0) {
                SetPointTemp = GetCurrentScheduleValue(IRink(SysNum).ColdSetptSchedPtr);
                OffTempCool = SetPointTemp - 0.5 * IRink(SysNum).ColdThrottlRange;
            } else { // This system is not capable of cooling, set OffTempCool to something really high
                OffTempCool = HighTempCooling;
            }

            if (ControlTemp > OffTempCool && IRink(SysNum).CoolingSystem) {
                OperatingMode = CoolingMode;
                ControlNode = IRink(SysNum).ColdBrineInNode;
                MaxRefrigFlow = IRink(SysNum).BrineFlowMaxCool;
                MassFlowFrac = (ControlTemp - OffTempCool) / IRink(SysNum).ColdThrottlRange;
            } else {
                MassFlowFrac = 0.0;
            }

            // Calculate and limit the refrigerant flow rate
            ActRefrigFlow = MassFlowFrac * MaxRefrigFlow;
            if (ActRefrigFlow < MassFlowTolerance) ActRefrigFlow = 0.0;
            if (OperatingMode == CoolingMode) {
                if (IRink(SysNum).CoolingSystem) {
                    SetComponentFlowRate(ActRefrigFlow,
                                         IRink(SysNum).ColdBrineInNode,
                                         IRink(SysNum).ColdBrineOutNode,
                                         IRink(SysNum).CBLoopNum,
                                         IRink(SysNum).CBLoopSide,
                                         IRink(SysNum).CBBranchNum,
                                         IRink(SysNum).CBCompNum);
                } else {
                    SysRunning = false;
                }
            }

            // Now simulate the system...
            if ((OperatingMode == CoolingMode) && SysRunning) CalcIndirectIndoorIceRinkComps(SysNum, LoadMet);
        }
    }

    void CalcIndirectIndoorIceRinkComps(int const SysNum,    // name of the indirect refrigeration system
                                        Real64 &LoadMet,     // load met by the indirect refrigeration system, in Watts
                                        int const RefrigType // Type of refrigerant used in the indirect type refrigeration system
    )
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
        int RefrigNodeIn;          // Node number of the refrigerant entering the refrigerant system
        int ZoneNum;               // Zone pointer for this refrigerant system
        int SurfNum;               // DO loop counter for the surfaces that comprise a particular radiant system
        int SurfNum2;              // Index to the floor in the surface derived type
        int SurfNum3;              // DO loop counter for the surfaces that comprise a particular radiant system
        int SurfNumC;              // Index to the surfaces used for condensation control
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
        Real64 Concentration;      // Concentration of the refrigerant used in the indirect type refrigeration system

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

        RefrigNodeIn = IRink(SysNum).ColdBrineInNode;
        Concentration = IRink(SysNum).GlycolConc;
        if (RefrigNodeIn == 0) {
            ShowSevereError("Illegal inlet node for the refrigerant in the direct system");
            ShowFatalError("Preceding condition causes termination");
        }

        ZoneNum = IRink(SysNum).ZonePtr;
        SysRefrigMassFlow = Node(RefrigNodeIn).MassFlowRate;
        RefrigMassFlow = Node(RefrigNodeIn).MassFlowRate / double(Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier);
        RefrigTempIn = Node(RefrigNodeIn).Temp;

        if (RefrigMassFlow <= 0) {
            // No flow or below minimum allowed so there is no heat source/sink
            // This is possible with a mismatch between system and plant operation
            // or a slight mismatch between zone and system controls.  This is not
            // necessarily a "problem" so this exception is necessary in the code.
            for (SurfNum = 1; SurfNum <= IRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(IRink(SysNum).SurfacePtr).Class == SurfaceClass_Floor) {
                    SurfNum2 = IRink(SysNum).SurfacePtr2(SurfNum);
                    QRadSysSource(SurfNum2) = 0.0;
                    if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                        QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                }
            }
        } else {
            for (SurfNum = 1; SurfNum <= IRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(IRink(SysNum).SurfacePtr).Class == SurfaceClass_Floor) {
                    // Determine the heat exchanger "effectiveness" term
                    EpsMdotCp = CalcIRinkHXEffectTerm(
                        RefrigTempIn, SysNum, RefrigMassFlow, IRink(SysNum).TubeLength, IRink(SysNum).TubeLength, RefrigType, Concentration);

                    ConstrNum = Surface(SurfNum).Construction;
                    if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CTF) {

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

            // "Temperature Comparision" Cut-off:
            for (SurfNum = 1; SurfNum <= IRink(SysNum).NumOfSurfaces; ++SurfNum) {
                if (Surface(IRink(SysNum).SurfacePtr).Class == SurfaceClass_Floor) {
                    // Check to see whether or not the system should really be running.  If
                    // QRadSysSource is positive when we are in cooling mode, then the radiant system
                    // will be doing the opposite of its intention.  In this case, the flow rate
                    // is set to zero to avoid heating in cooling mode
                    if ((OperatingMode == CoolingMode) && (QRadSysSource(SurfNum) >= 0.0)) {
                        RefrigMassFlow = 0.0;
                        SetComponentFlowRate(RefrigMassFlow,
                                             IRink(SysNum).ColdBrineInNode,
                                             IRink(SysNum).ColdBrineOutNode,
                                             IRink(SysNum).CBLoopNum,
                                             IRink(SysNum).CBLoopSide,
                                             IRink(SysNum).CBBranchNum,
                                             IRink(SysNum).CBCompNum);
                    }
                    IRink(SysNum).BrineMassFlowRate = RefrigMassFlow;

                    for (SurfNum2 = 1; SurfNum2 <= IRink(SysNum).NumOfSurfaces; ++SurfNum2) {
                        SurfNum2 = IRink(SysNum).SurfacePtr2(SurfNum2);
                        QRadSysSource(SurfNum2) = 0.0;
                        if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                            QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                    }
                    break;
                }
            }

            // Condensation Cut-off:
            // Check to see whether there are any surface temperatures within the radiant system that have
            // dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
            // A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
            // conditions.
            IRink(SysNum).CondCausedShutDown = false;
            DewPointTemp = PsyTdpFnWPb(ZoneAirHumRat(ZoneNum), OutBaroPress);

            if ((OperatingMode == CoolingMode) && (IRink(SysNum).CondCtrlType == CondCtrlSimpleOff)) {
                for (SurfNumC = 1; SurfNumC <= IRink(SysNum).NumOfSurfaces; ++SurfNumC) {
                    if (TH(2, 1, IRink(SysNum).SurfacePtr2(SurfNumC)) < (DewPointTemp + IRink(SysNum).CondDewPtDeltaT)) {
                        // Condensation warning--must shut off radiant system
                        IRink(SysNum).CondCausedShutDown = true;
                        RefrigMassFlow = 0.0;
                        SetComponentFlowRate(RefrigMassFlow,
                                             IRink(SysNum).ColdBrineInNode,
                                             IRink(SysNum).ColdBrineOutNode,
                                             IRink(SysNum).CBLoopNum,
                                             IRink(SysNum).CBLoopSide,
                                             IRink(SysNum).CBBranchNum,
                                             IRink(SysNum).CBCompNum);
                        IRink(SysNum).BrineMassFlowRate = RefrigMassFlow;

                        // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                        if (!WarmupFlag) {
                            if (IRink(SysNum).CondErrIndex == 0) {
                                ShowWarningMessage(cDRink + " [" + IRink(SysNum).Name + ']');
                                ShowContinueError("Surface [" + Surface(IRink(SysNum).SurfacePtr2(SurfNumC)).Name +
                                                  "] temperature below dew-point temperature--potential for condensation exists");
                                ShowContinueError("Flow to the radiant system will be shut-off to avoid condensation");
                                ShowContinueError("Predicted radiant system surface temperature = " +
                                                  RoundSigDigits(TH(2, 1, IRink(SysNum).SurfacePtr2(SurfNumC)), 2));
                                ShowContinueError("Zone dew-point temperature + safety delta T= " +
                                                  RoundSigDigits(DewPointTemp + IRink(SysNum).CondDewPtDeltaT, 2));
                                ShowContinueErrorTimeStamp("");
                                ShowContinueError("Note that a " + RoundSigDigits(IRink(SysNum).CondDewPtDeltaT, 4) +
                                                  " C safety was chosen in the input for the shut-off criteria");
                            }
                            ShowRecurringWarningErrorAtEnd(cDRink + " [" + IRink(SysNum).Name + "] condensation shut-off occurrence continues.",
                                                           IRink(SysNum).CondErrIndex,
                                                           DewPointTemp,
                                                           DewPointTemp,
                                                           _,
                                                           "C",
                                                           "C");
                        }
                        break; // outer do loop
                    }
                }
            } else if ((OperatingMode == CoolingMode) && (IRink(SysNum).CondCtrlType == CondCtrlNone)) {
                for (SurfNumC = 1; SurfNumC <= IRink(SysNum).NumOfSurfaces; ++SurfNumC) {
                    if (TH(2, 1, IRink(SysNum).SurfacePtr2(SurfNumC)) < DewPointTemp) {
                        // Condensation occuring but user does not want to shut down the system off ever
                        IRink(SysNum).CondCausedShutDown = true;
                    }
                }
            } else if ((OperatingMode == CoolingMode) && (IRink(SysNum).CondCtrlType == CondCtrlVariedOff)) {
                LowestRadSurfTemp = 999.9;
                CondSurfNum = 0;
                for (SurfNumC = 1; SurfNumC <= IRink(SysNum).NumOfSurfaces; ++SurfNumC) {
                    if (TH(2, 1, IRink(SysNum).SurfacePtr2(SurfNumC)) < (DewPointTemp + IRink(SysNum).CondDewPtDeltaT)) {
                        if (TH(2, 1, IRink(SysNum).SurfacePtr2(SurfNumC)) < LowestRadSurfTemp) {
                            LowestRadSurfTemp = TH(2, 1, IRink(SysNum).SurfacePtr2(SurfNumC));
                            CondSurfNum = SurfNumC;
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
                                         IRink(SysNum).ColdBrineInNode,
                                         IRink(SysNum).ColdBrineOutNode,
                                         IRink(SysNum).CBLoopNum,
                                         IRink(SysNum).CBLoopSide,
                                         IRink(SysNum).CBBranchNum,
                                         IRink(SysNum).CBCompNum);
                    IRink(SysNum).BrineMassFlowRate = RefrigMassFlow;
                    for (SurfNum3 = 1; SurfNum3 <= IRink(SysNum).NumOfSurfaces; ++SurfNum3) {
                        SurfNum2 = IRink(SysNum).SurfacePtr2(SurfNum3);
                        QRadSysSource(SurfNum2) = 0.0;
                        if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                            QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                    }
                    // Redo the heat balances since we have changed the heat source (set it to zero)
                    HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(ZoneNum);
                    HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(ZoneNum);
                    // Now check all of the surface temperatures.  If any potentially have condensation, leave the system off.
                    for (SurfNum2 = 1; SurfNum2 <= IRink(SysNum).NumOfSurfaces; ++SurfNum2) {
                        if (TH(2, 1, IRink(SysNum).SurfacePtr2(SurfNum2)) < (DewPointTemp + IRink(SysNum).CondDewPtDeltaT)) {
                            IRink(SysNum).CondCausedShutDown = true;
                        }
                    }
                    // If the system does not need to be shut down, then let's see if we can vary the flow based
                    // on the lowest temperature surface from before.  This will use interpolation to try a new
                    // flow rate.
                    if (!IRink(SysNum).CondCausedShutDown) {
                        PredictedCondTemp = DewPointTemp + IRink(SysNum).CondDewPtDeltaT;
                        ZeroFlowSurfTemp = TH(2, 1, IRink(SysNum).SurfacePtr2(CondSurfNum));
                        ReductionFrac = (ZeroFlowSurfTemp - PredictedCondTemp) / std::abs(ZeroFlowSurfTemp - LowestRadSurfTemp);
                        if (ReductionFrac < 0.0) ReductionFrac = 0.0; // Shouldn't happen as the above check should have screened this out
                        if (ReductionFrac > 1.0) ReductionFrac = 1.0; // Shouldn't happen either because condensation doesn't exist then
                        RefrigMassFlow = ReductionFrac * FullRefrigMassFlow;
                        SysRefrigMassFlow = double(Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier) * RefrigMassFlow;
                        // Got a new reduced flow rate that should work...reset loop variable and resimulate the system
                        SetComponentFlowRate(SysRefrigMassFlow,
                                             IRink(SysNum).ColdBrineInNode,
                                             IRink(SysNum).ColdBrineOutNode,
                                             IRink(SysNum).CBLoopNum,
                                             IRink(SysNum).CBLoopSide,
                                             IRink(SysNum).CBBranchNum,
                                             IRink(SysNum).CBCompNum);
                        IRink(SysNum).BrineMassFlowRate = SysRefrigMassFlow;
                        // Go through all of the surfaces again with the new flow rate...
                        for (SurfNum3 = 1; SurfNum3 <= IRink(SysNum).NumOfSurfaces; ++SurfNum3) {
                            SurfNum = IRink(SysNum).SurfacePtr2(SurfNum3);
                            // Determine the heat exchanger "effectiveness" term
                            EpsMdotCp = CalcIRinkHXEffectTerm(
                                RefrigTempIn, SysNum, RefrigMassFlow, IRink(SysNum).TubeLength, IRink(SysNum).TubeLength, RefrigType, Concentration);
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
                        // Redo the heat balances since we have changed the heat source
                        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(ZoneNum);
                        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(ZoneNum);

                        // Check for condensation one more time.  If no condensation, we are done.  If there is
                        // condensation, shut things down and be done.
                        for (SurfNum2 = 1; SurfNum2 <= IRink(SysNum).NumOfSurfaces; ++SurfNum2) {
                            if (IRink(SysNum).CondCausedShutDown) break;
                            if (TH(2, 1, IRink(SysNum).SurfacePtr2(SurfNum2)) < (PredictedCondTemp)) {
                                // Condensation still present--must shut off radiant system
                                IRink(SysNum).CondCausedShutDown = true;
                                RefrigMassFlow = 0.0;
                                SurfNum = SurfNum2;
                                SetComponentFlowRate(SysRefrigMassFlow,
                                                     IRink(SysNum).ColdBrineInNode,
                                                     IRink(SysNum).ColdBrineOutNode,
                                                     IRink(SysNum).CBLoopNum,
                                                     IRink(SysNum).CBLoopSide,
                                                     IRink(SysNum).CBBranchNum,
                                                     IRink(SysNum).CBCompNum);
                                IRink(SysNum).BrineMassFlowRate = RefrigMassFlow;
                                for (SurfNum3 = 1; SurfNum3 <= IRink(SysNum).NumOfSurfaces; ++SurfNum3) {
                                    SurfNum2 = IRink(SysNum).SurfacePtr2(SurfNum3);
                                    QRadSysSource(SurfNum2) = 0.0;
                                    if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                                        QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                                }
                            }
                        }
                    }

                    if (IRink(SysNum).CondCausedShutDown) {
                        // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                        if (!WarmupFlag) {
                            if (IRink(SysNum).CondErrIndex == 0) {
                                ShowWarningMessage(cDRink + " [" + IRink(SysNum).Name + ']');
                                ShowContinueError("Surface [" + Surface(IRink(SysNum).SurfacePtr2(CondSurfNum)).Name +
                                                  "] temperature below dew-point temperature--potential for condensation exists");
                                ShowContinueError("Flow to the radiant system will be shut-off to avoid condensation");
                                ShowContinueError("Predicted radiant system surface temperature = " +
                                                  RoundSigDigits(TH(2, 1, IRink(SysNum).SurfacePtr2(CondSurfNum)), 2));
                                ShowContinueError("Zone dew-point temperature + safety delta T= " +
                                                  RoundSigDigits(DewPointTemp + IRink(SysNum).CondDewPtDeltaT, 2));
                                ShowContinueErrorTimeStamp("");
                                ShowContinueError("Note that a " + RoundSigDigits(IRink(SysNum).CondDewPtDeltaT, 4) +
                                                  " C safety was chosen in the input for the shut-off criteria");
                            }
                            ShowRecurringWarningErrorAtEnd(cDRink + " [" + IRink(SysNum).Name + "] condensation shut-off occurrence continues.",
                                                           IRink(SysNum).CondErrIndex,
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

        CpRefrig = 2200.0;

        // Claculate the reynold's number
        ReD = 4.0 * RefrigMassFlow / (Pi * MUactual * TubeDiameter);

        if (ReD >= MaxLaminarRe) { // Turbulent flow --> use Colburn equation

            NuD = 0.023 * std::pow(ReD, 0.8) * std::pow(PRactual, 1.0 / 3.0);

        } else { // Laminar flow --> use constant surface temperature relation

            NuD = 3.66;
        }

        NTU = Pi * Kactual * TubeLength / (RefrigMassFlow * CpRefrig);
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
            NTU = Pi * Kactual * TubeLength / (RefrigMassFlow * CpCacl2);
        } else {
            NTU = Pi * Kactual * TubeLength / (RefrigMassFlow * CpEG);
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

    void UpdateRadSysSourceValAvg(bool &LowTempRadSysOn) // .TRUE. if the radiant system has run this zone time step
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   November 2000

        // PURPOSE OF THIS SUBROUTINE:
        // To transfer the average value of the heat source/sink over the entire
        // zone time step back to the heat balance routines so that the heat
        // balance algorithms can simulate one last time with the average source
        // to maintain some reasonable amount of continuity and energy balance
        // in the temperature and flux histories.

        // METHODOLOGY EMPLOYED:
        // All of the record keeping for the average term is done in the Update
        // routine so the only other thing that this subroutine does is check to
        // see if the system was even on.  If any average term is non-zero, then
        // one or more of the radiant systems was running.

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CloseEnough(0.01); // Some arbitrarily small value to avoid zeros and numbers that are almost the same

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // DO loop counter for surface index

        LowTempRadSysOn = false;

        // If this was never allocated, then there are no radiant systems in this input file (just RETURN)
        if (!allocated(QRadSysSrcAvg)) return;

        // If it was allocated, then we have to check to see if this was running at all...
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (QRadSysSrcAvg(SurfNum) != 0.0) {
                LowTempRadSysOn = true;
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

} // namespace IceRink
} // namespace EnergyPlus
