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

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataConversions.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SwimmingPool.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace SwimmingPool {

    // MODULE INFORMATION:
    //       AUTHOR         Rick Strand, Ho-Sung Kim
    //       DATE WRITTEN   June 2012 (F90) and October 2014 (C++)

    // PURPOSE OF THIS MODULE:
    // The purpose of this module is to encapsulate the data and algorithms required
    // to manage the SwimmingPool System Component.

    // METHODOLOGY EMPLOYED:
    // The swimming pool acts as a surface within the heat balance and then connects
    // to the plant via a water loop.

    // REFERENCES:
    // 1. ASHRAE (2011). 2011 ASHRAE Handbook - HVAC Applications. Atlanta: American Society of Heating,
    //    Refrigerating and Air-Conditioning Engineers, Inc., p.5.6-5.9.
    // 2. Janis, R. and W. Tao (2005). Mechanical and Electrical Systems in Buildings. 3rd ed. Upper
    //    Saddle River, NJ: Pearson Education, Inc., p.246.
    // 3. Kittler, R. (1989). Indoor Natatorium Design and Energy Recycling. ASHRAE Transactions 95(1), p.521-526.
    // 4. Smith, C., R. Jones, and G. Lof (1993). Energy Requirements and Potential Savings for Heated
    //    Indoor Swimming Pools. ASHRAE Transactions 99(2), p.864-874.

    static std::string const BlankString;

    // MODULE VARIABLE DECLARATIONS:
    int NumSwimmingPools(0); // Number of swimming pools
    Array1D_bool CheckEquipName;
    bool getSwimmingPoolInput = true;
    Array1D<SwimmingPoolData> Pool;

    void clear_state()
    {
        NumSwimmingPools = 0;
        getSwimmingPoolInput = true;
        CheckEquipName.deallocate();
        Pool.deallocate();
    }

    void SimSwimmingPool(EnergyPlusData &state, bool FirstHVACIteration)
    {
        // Process the input data if it hasn't been done already
        if (getSwimmingPoolInput) {
            GetSwimmingPool();
            getSwimmingPoolInput = false;
        }

        // System wide (for all pools) inits
        DataHeatBalFanSys::SumConvPool = 0.0;
        DataHeatBalFanSys::SumLatentPool = 0.0;

        PlantLocation A(0, 0, 0, 0);
        Real64 CurLoad = 0.0;
        bool RunFlag = true;

        for (auto &thisPool : Pool) {
            thisPool.simulate(state, A, FirstHVACIteration, CurLoad, RunFlag);
        }

        if (NumSwimmingPools > 0) HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state.dataWindowManager);

        ReportSwimmingPool();
    }

    void SwimmingPoolData::simulate(EnergyPlusData &state, const PlantLocation &EP_UNUSED(calledFromLocation),
                                    bool FirstHVACIteration,
                                    Real64 &EP_UNUSED(CurLoad),
                                    bool EP_UNUSED(RunFlag))
    {
        this->initialize(state.dataBranchInputManager, FirstHVACIteration);

        this->calculate();

        this->update();
    }

    void GetSwimmingPool()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand, Ho-Sung Kim
        //       DATE WRITTEN   October 2014

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reads the input for all swimming pools present in
        // the user input file.  This will contain all of the information needed
        // to simulate a swimming pool.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetSwimmingPool: "); // include trailing blank space
        Real64 const MinCoverFactor(0.0);                          // minimum value for cover factors
        Real64 const MaxCoverFactor(1.0);                          // maximum value for cover factors
        Real64 const MinDepth(0.05);                               // minimum average pool depth (to avoid obvious input errors)
        Real64 const MaxDepth(10.0);                               // maximum average pool depth (to avoid obvious input errors)
        Real64 const MinPowerFactor(0.0);                          // minimum power factor for miscellaneous equipment

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound(false);         // Set to true if something goes wrong
        std::string CurrentModuleObject; // for ease in getting objects
        Array1D_string Alphas;           // Alpha items for object
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        int IOStatus;                    // Used in GetObjectItem
        int Item;                        // Item to be "gotten"
        Array1D<Real64> Numbers;         // Numeric items for object
        int NumAlphas;                   // Number of Alphas for each GetObjectItem call
        int NumArgs;                     // Unused variable that is part of a subroutine call
        int NumNumbers;                  // Number of Numbers for each GetObjectItem call
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.

        // Initializations and allocations
        int MaxAlphas = 0;  // Maximum number of alphas for these input keywords
        int MaxNumbers = 0; // Maximum number of numbers for these input keywords

        inputProcessor->getObjectDefMaxArgs("SwimmingPool:Indoor", NumArgs, NumAlphas, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNumbers);

        Alphas.allocate(MaxAlphas);
        Alphas = "";
        Numbers.allocate(MaxNumbers);
        Numbers = 0.0;
        cAlphaFields.allocate(MaxAlphas);
        cAlphaFields = "";
        cNumericFields.allocate(MaxNumbers);
        cNumericFields = "";
        lAlphaBlanks.allocate(MaxAlphas);
        lAlphaBlanks = true;
        lNumericBlanks.allocate(MaxNumbers);
        lNumericBlanks = true;

        NumSwimmingPools = inputProcessor->getNumObjectsFound("SwimmingPool:Indoor");
        CheckEquipName.allocate(NumSwimmingPools);
        CheckEquipName = true;

        Pool.allocate(NumSwimmingPools);

        // Obtain all of the user data related to indoor swimming pools...
        CurrentModuleObject = "SwimmingPool:Indoor";
        for (Item = 1; Item <= NumSwimmingPools; ++Item) {

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
            UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
            Pool(Item).Name = Alphas(1);

            Pool(Item).SurfaceName = Alphas(2);
            Pool(Item).SurfacePtr = 0;
            for (int SurfNum = 1; SurfNum <= DataSurfaces::TotSurfaces; ++SurfNum) {
                if (UtilityRoutines::SameString(DataSurfaces::Surface(SurfNum).Name, Pool(Item).SurfaceName)) {
                    Pool(Item).SurfacePtr = SurfNum;
                    break;
                }
            }

            Pool(Item).ErrorCheckSetupPoolSurface(Alphas(1),Alphas(2),cAlphaFields(2),ErrorsFound);

            Pool(Item).AvgDepth = Numbers(1);
            if (Pool(Item).AvgDepth < MinDepth) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + " has an average depth that is too small.");
                ShowContinueError("The pool average depth has been reset to the minimum allowed depth.");
            } else if (Pool(Item).AvgDepth > MaxDepth) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + " has an average depth that is too large.");
                ShowContinueError("The pool depth must be less than the maximum average depth of 10 meters.");
                ErrorsFound = true;
            }

            Pool(Item).ActivityFactorSchedName = Alphas(3);
            Pool(Item).ActivityFactorSchedPtr = ScheduleManager::GetScheduleIndex(Alphas(3));
            if ((Pool(Item).ActivityFactorSchedPtr == 0) && (!lAlphaBlanks(3))) {
                ShowSevereError(cAlphaFields(3) + " not found: " + Alphas(3));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            Pool(Item).MakeupWaterSupplySchedName = Alphas(4);
            Pool(Item).MakeupWaterSupplySchedPtr = ScheduleManager::GetScheduleIndex(Alphas(4));
            if ((Pool(Item).MakeupWaterSupplySchedPtr == 0) && (!lAlphaBlanks(4))) {
                ShowSevereError(cAlphaFields(4) + " not found: " + Alphas(4));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            Pool(Item).CoverSchedName = Alphas(5);
            Pool(Item).CoverSchedPtr = ScheduleManager::GetScheduleIndex(Alphas(5));
            if ((Pool(Item).CoverSchedPtr == 0) && (!lAlphaBlanks(5))) {
                ShowSevereError(cAlphaFields(5) + " not found: " + Alphas(5));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            Pool(Item).CoverEvapFactor = Numbers(2);
            if (Pool(Item).CoverEvapFactor < MinCoverFactor) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + " has an evaporation cover factor less than zero.");
                ShowContinueError("The evaporation cover factor has been reset to zero.");
                Pool(Item).CoverEvapFactor = MinCoverFactor;
            } else if (Pool(Item).CoverEvapFactor > MaxCoverFactor) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + " has an evaporation cover factor greater than one.");
                ShowContinueError("The evaporation cover factor has been reset to one.");
                Pool(Item).CoverEvapFactor = MaxCoverFactor;
            }

            Pool(Item).CoverConvFactor = Numbers(3);
            if (Pool(Item).CoverConvFactor < MinCoverFactor) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + " has a convection cover factor less than zero.");
                ShowContinueError("The convection cover factor has been reset to zero.");
                Pool(Item).CoverConvFactor = MinCoverFactor;
            } else if (Pool(Item).CoverConvFactor > MaxCoverFactor) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + " has a convection cover factor greater than one.");
                ShowContinueError("The convection cover factor has been reset to one.");
                Pool(Item).CoverConvFactor = MaxCoverFactor;
            }

            Pool(Item).CoverSWRadFactor = Numbers(4);
            if (Pool(Item).CoverSWRadFactor < MinCoverFactor) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " has a short-wavelength radiation cover factor less than zero.");
                ShowContinueError("The short-wavelength radiation cover factor has been reset to zero.");
                Pool(Item).CoverSWRadFactor = MinCoverFactor;
            } else if (Pool(Item).CoverSWRadFactor > MaxCoverFactor) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " has a short-wavelength radiation cover factor greater than one.");
                ShowContinueError("The short-wavelength radiation cover factor has been reset to one.");
                Pool(Item).CoverSWRadFactor = MaxCoverFactor;
            }

            Pool(Item).CoverLWRadFactor = Numbers(5);
            if (Pool(Item).CoverLWRadFactor < MinCoverFactor) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " has a long-wavelength radiation cover factor less than zero.");
                ShowContinueError("The long-wavelength radiation cover factor has been reset to zero.");
                Pool(Item).CoverLWRadFactor = MinCoverFactor;
            } else if (Pool(Item).CoverLWRadFactor > MaxCoverFactor) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) +
                                 " has a long-wavelength radiation cover factor greater than one.");
                ShowContinueError("The long-wavelength radiation cover factor has been reset to one.");
                Pool(Item).CoverLWRadFactor = MaxCoverFactor;
            }

            Pool(Item).WaterInletNodeName = Alphas(6);
            Pool(Item).WaterOutletNodeName = Alphas(7);
            Pool(Item).WaterInletNode = NodeInputManager::GetOnlySingleNode(Alphas(6),
                                                                            ErrorsFound,
                                                                            CurrentModuleObject,
                                                                            Alphas(1),
                                                                            DataLoopNode::NodeType_Water,
                                                                            DataLoopNode::NodeConnectionType_Inlet,
                                                                            1,
                                                                            DataLoopNode::ObjectIsNotParent);
            Pool(Item).WaterOutletNode = NodeInputManager::GetOnlySingleNode(Alphas(7),
                                                                             ErrorsFound,
                                                                             CurrentModuleObject,
                                                                             Alphas(1),
                                                                             DataLoopNode::NodeType_Water,
                                                                             DataLoopNode::NodeConnectionType_Outlet,
                                                                             1,
                                                                             DataLoopNode::ObjectIsNotParent);
            if ((!lAlphaBlanks(6)) || (!lAlphaBlanks(7))) {
                BranchNodeConnections::TestCompSet(CurrentModuleObject, Alphas(1), Alphas(6), Alphas(7), "Hot Water Nodes");
            }
            Pool(Item).WaterVolFlowMax = Numbers(6);
            Pool(Item).MiscPowerFactor = Numbers(7);
            if (Pool(Item).MiscPowerFactor < MinPowerFactor) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + " has a miscellaneous power factor less than zero.");
                ShowContinueError("The miscellaneous power factor has been reset to zero.");
                Pool(Item).MiscPowerFactor = MinPowerFactor;
            }

            Pool(Item).SetPtTempSchedName = Alphas(8);
            Pool(Item).SetPtTempSchedPtr = ScheduleManager::GetScheduleIndex(Alphas(8));
            if ((Pool(Item).SetPtTempSchedPtr == 0) && (!lAlphaBlanks(8))) {
                ShowSevereError(cAlphaFields(8) + " not found: " + Alphas(8));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
            if (lAlphaBlanks(8)) {
                ShowSevereError(cAlphaFields(8) + " left blank.  This is NOT allowed as there must be a pool water setpoint temperature.");
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            Pool(Item).MaxNumOfPeople = Numbers(8);
            if (Pool(Item).MaxNumOfPeople < 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + " was entered with negative people.  This is not allowed.");
                ShowContinueError("The number of people has been reset to zero.");
                Pool(Item).MaxNumOfPeople = 0.0;
            }

            Pool(Item).PeopleSchedName = Alphas(9);
            Pool(Item).PeopleSchedPtr = ScheduleManager::GetScheduleIndex(Alphas(9));
            if ((Pool(Item).PeopleSchedPtr == 0) && (!lAlphaBlanks(9))) {
                ShowSevereError(cAlphaFields(9) + " not found: " + Alphas(9));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            Pool(Item).PeopleHeatGainSchedName = Alphas(10);
            Pool(Item).PeopleHeatGainSchedPtr = ScheduleManager::GetScheduleIndex(Alphas(10));
            if ((Pool(Item).PeopleHeatGainSchedPtr == 0) && (!lAlphaBlanks(10))) {
                ShowSevereError(cAlphaFields(10) + " not found: " + Alphas(10));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
        }

        Alphas.deallocate();
        Numbers.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in swimming pool input. Preceding conditions cause termination.");
        }
    }

    void SwimmingPoolData::ErrorCheckSetupPoolSurface(std::string const Alpha1,
                                                      std::string const Alpha2,
                                                      std::string const cAlphaField2,
                                                      bool &ErrorsFound
    )
    {

        static std::string const RoutineName("ErrorCheckSetupPoolSurface: "); // include trailing blank space
        static std::string const CurrentModuleObject("SwimmingPool:Indoor");

        if (this->SurfacePtr <= 0) {
            ShowSevereError(RoutineName + "Invalid " + cAlphaField2 + " = " + Alpha2);
            ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alpha1);
            ErrorsFound = true;
        } else if (DataSurfaces::Surface(this->SurfacePtr).IsRadSurfOrVentSlabOrPool) {
            ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alpha1 + "\", Invalid Surface");
            ShowContinueError(cAlphaField2 + "=\"" + Alpha2 + "\" has been used in another radiant system, ventilated slab, or pool.");
            ShowContinueError(
                "A single surface can only be a radiant system, a ventilated slab, or a pool.  It CANNOT be more than one of these.");
            ErrorsFound = true;
            // Something present that is not allowed for a swimming pool (non-CTF algorithm, movable insulation, or radiant source/sink
        } else if (DataSurfaces::Surface(this->SurfacePtr).HeatTransferAlgorithm != DataSurfaces::HeatTransferModel_CTF) {
            ShowSevereError(DataSurfaces::Surface(this->SurfacePtr).Name +
                            " is a pool and is attempting to use a non-CTF solution algorithm.  This is "
                            "not allowed.  Use the CTF solution algorithm for this surface.");
            ErrorsFound = true;
        } else if (DataSurfaces::Surface(this->SurfacePtr).Class == DataSurfaces::SurfaceClass_Window) {
            ShowSevereError(DataSurfaces::Surface(this->SurfacePtr).Name +
                            " is a pool and is defined as a window.  This is not allowed.  A pool must be a floor that is NOT a window.");
            ErrorsFound = true;
        } else if (DataSurfaces::Surface(this->SurfacePtr).MaterialMovInsulInt > 0) {
            ShowSevereError(DataSurfaces::Surface(this->SurfacePtr).Name +
                            " is a pool and has movable insulation.  This is not allowed.  Remove the movable insulation for this surface.");
            ErrorsFound = true;
        } else if (DataHeatBalance::Construct(DataSurfaces::Surface(this->SurfacePtr).Construction).SourceSinkPresent) {
            ShowSevereError(
                DataSurfaces::Surface(this->SurfacePtr).Name +
                " is a pool and uses a construction with a source/sink.  This is not allowed.  Use a standard construction for this surface.");
            ErrorsFound = true;
        } else { // ( Pool( Item ).SurfacePtr > 0 )
            DataSurfaces::Surface(this->SurfacePtr).IsRadSurfOrVentSlabOrPool = true;
            DataSurfaces::Surface(this->SurfacePtr).IsPool = true;
            this->ZonePtr = DataSurfaces::Surface(this->SurfacePtr).Zone;
            // Check to make sure pool surface is a floor
            if (DataSurfaces::Surface(this->SurfacePtr).Class != DataSurfaces::SurfaceClass_Floor) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alpha1 + " contains a surface name that is NOT a floor.");
                ShowContinueError(
                    "A swimming pool must be associated with a surface that is a FLOOR.  Association with other surface types is not permitted.");
                ErrorsFound = true;
            }
        }
    }

    void SwimmingPoolData::initialize(BranchInputManagerData &dataBranchInputManager, bool const FirstHVACIteration // true during the first HVAC iteration
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand, Ho-Sung Kim
        //       DATE WRITTEN   October 2014

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes variables relating to indoor swimming pools.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitSwimmingPool");
        Real64 const MinActivityFactor = 0.0;  // Minimum value for activity factor
        Real64 const MaxActivityFactor = 10.0; // Maximum value for activity factor (realistically)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 HeatGainPerPerson = ScheduleManager::GetCurrentScheduleValue(this->PeopleHeatGainSchedPtr);
        Real64 PeopleModifier = ScheduleManager::GetCurrentScheduleValue(this->PeopleSchedPtr);

        if (this->MyOneTimeFlag) {
            this->setupOutputVars(); // Set up the output variables once here
            this->ZeroSourceSumHATsurf.allocate(DataGlobals::NumOfZones);
            this->ZeroSourceSumHATsurf = 0.0;
            this->QPoolSrcAvg.allocate(DataSurfaces::TotSurfaces);
            this->QPoolSrcAvg = 0.0;
            this->HeatTransCoefsAvg.allocate(DataSurfaces::TotSurfaces);
            this->HeatTransCoefsAvg = 0.0;
            this->LastQPoolSrc.allocate(DataSurfaces::TotSurfaces);
            this->LastQPoolSrc = 0.0;
            this->LastHeatTransCoefs.allocate(DataSurfaces::TotSurfaces);
            this->LastHeatTransCoefs = 0.0;
            this->LastSysTimeElapsed.allocate(DataSurfaces::TotSurfaces);
            this->LastSysTimeElapsed = 0.0;
            this->LastTimeStepSys.allocate(DataSurfaces::TotSurfaces);
            this->LastTimeStepSys = 0.0;
            this->MyOneTimeFlag = false;
        }

        SwimmingPoolData::initSwimmingPoolPlantLoopIndex(dataBranchInputManager);

        if (DataGlobals::BeginEnvrnFlag && this->MyEnvrnFlagGeneral) {
            this->ZeroSourceSumHATsurf = 0.0;
            this->QPoolSrcAvg = 0.0;
            this->HeatTransCoefsAvg = 0.0;
            this->LastQPoolSrc = 0.0;
            this->LastHeatTransCoefs = 0.0;
            this->LastSysTimeElapsed = 0.0;
            this->LastTimeStepSys = 0.0;
            this->MyEnvrnFlagGeneral = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) this->MyEnvrnFlagGeneral = true;

        if (DataGlobals::BeginEnvrnFlag) {
            this->PoolWaterTemp = 23.0;
            this->HeatPower = 0.0;
            this->HeatEnergy = 0.0;
            this->MiscEquipPower = 0.0;
            this->MiscEquipEnergy = 0.0;
            this->WaterInletTemp = 0.0;
            this->WaterOutletTemp = 0.0;
            this->WaterMassFlowRate = 0.0;
            this->PeopleHeatGain = 0.0;
            Real64 Density = FluidProperties::GetDensityGlycol("WATER", this->PoolWaterTemp, this->GlycolIndex, RoutineName);
            this->WaterMass = DataSurfaces::Surface(this->SurfacePtr).Area * this->AvgDepth * Density;
            this->WaterMassFlowRateMax = this->WaterVolFlowMax * Density;
            initSwimmingPoolPlantNodeFlow(this->MyPlantScanFlagPool);
        }

        if (DataGlobals::BeginTimeStepFlag && FirstHVACIteration) { // This is the first pass through in a particular time step

            int ZoneNum = this->ZonePtr;
            this->ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum); // Set this to figure what part of the load the radiant system meets
            int SurfNum = this->SurfacePtr;
            this->QPoolSrcAvg(SurfNum) = 0.0;        // Initialize this variable to zero (pool parameters "off")
            this->HeatTransCoefsAvg(SurfNum) = 0.0;  // Initialize this variable to zero (pool parameters "off")
            this->LastQPoolSrc(SurfNum) = 0.0;       // At the start of a time step, reset to zero so average calculation can begin again
            this->LastSysTimeElapsed(SurfNum) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
            this->LastTimeStepSys(SurfNum) = 0.0;    // At the start of a time step, reset to zero so average calculation can begin again
        }

        // initialize the flow rate for the component on the plant side (this follows standard procedure for other components like low temperature
        // radiant systems)
        Real64 mdot = 0.0;
        PlantUtilities::SetComponentFlowRate(
            mdot, this->WaterInletNode, this->WaterOutletNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchNum, this->HWCompNum);
        this->WaterInletTemp = DataLoopNode::Node(this->WaterInletNode).Temp;

        // get the schedule values for different scheduled parameters
        if (this->ActivityFactorSchedPtr > 0) {
            this->CurActivityFactor = ScheduleManager::GetCurrentScheduleValue(this->ActivityFactorSchedPtr);
            if (this->CurActivityFactor < MinActivityFactor) {
                this->CurActivityFactor = MinActivityFactor;
                ShowWarningError(RoutineName + ": Swimming Pool =\"" + this->Name + " Activity Factor Schedule =\"" + this->ActivityFactorSchedName +
                                 " has a negative value.  This is not allowed.");
                ShowContinueError("The activity factor has been reset to zero.");
            }
            if (this->CurActivityFactor > MaxActivityFactor) {
                this->CurActivityFactor = 1.0;
                ShowWarningError(RoutineName + ": Swimming Pool =\"" + this->Name + " Activity Factor Schedule =\"" + this->ActivityFactorSchedName +
                                 " has a value larger than 10.  This is not allowed.");
                ShowContinueError("The activity factor has been reset to unity.");
            }
        } else {
            // default is activity factor of 1.0
            this->CurActivityFactor = 1.0;
        }

        this->CurSetPtTemp = ScheduleManager::GetCurrentScheduleValue(this->SetPtTempSchedPtr);

        if (this->MakeupWaterSupplySchedPtr > 0) {
            this->CurMakeupWaterTemp = ScheduleManager::GetCurrentScheduleValue(this->MakeupWaterSupplySchedPtr);
        } else {
            // use water main temperaure if no schedule present in input
            this->CurMakeupWaterTemp = DataEnvironment::WaterMainsTemp;
        }

        // determine the current heat gain from people
        if (this->PeopleHeatGainSchedPtr > 0) {
            if (HeatGainPerPerson < 0.0) {
                ShowWarningError(RoutineName + ": Swimming Pool =\"" + this->Name + " Heat Gain Schedule =\"" + this->PeopleHeatGainSchedName +
                                 " has a negative value.  This is not allowed.");
                ShowContinueError("The heat gain per person has been reset to zero.");
                HeatGainPerPerson = 0.0;
            }
            if (this->PeopleSchedPtr > 0) {
                if (PeopleModifier < 0.0) {
                    ShowWarningError(RoutineName + ": Swimming Pool =\"" + this->Name + " People Schedule =\"" + this->PeopleSchedName +
                                     " has a negative value.  This is not allowed.");
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
        this->PeopleHeatGain = PeopleModifier * HeatGainPerPerson * this->MaxNumOfPeople;

        // once cover schedule value is established, define the current values of the cover heat transfer factors
        if (this->CoverSchedPtr > 0) {
            this->CurCoverSchedVal = ScheduleManager::GetCurrentScheduleValue(this->CoverSchedPtr);
            if (this->CurCoverSchedVal > 1.0) {
                ShowWarningError(RoutineName + ": Swimming Pool =\"" + this->Name + " Cover Schedule =\"" + this->CoverSchedName +
                                 " has a value greater than 1.0 (100%).  This is not allowed.");
                ShowContinueError("The cover has been reset to one or fully covered.");
                this->CurCoverSchedVal = 1.0;
            } else if (this->CurCoverSchedVal < 0.0) {
                ShowWarningError(RoutineName + ": Swimming Pool =\"" + this->Name + " Cover Schedule =\"" + this->CoverSchedName +
                                 " has a negative value.  This is not allowed.");
                ShowContinueError("The cover has been reset to zero or uncovered.");
                this->CurCoverSchedVal = 0.0;
            }
        } else {
            // default is NO pool cover
            this->CurCoverSchedVal = 0.0;
        }
        // for the current cover factors, a value of 1.0 means that the pool is open (not covered)
        // the user input values determine the amount the pool cover degrades one of the factors
        // for example, if the cover reduces convection by 50% and the pool is half covered, then
        // the reduction factor for convection is 25% or 75% of the normal value.  this establishes
        // the following relationships and how they are used in other parts of the code.
        // note that for the radiation factors, the reduction in absorption of radiation caused by
        // the cover will result in a net imbalance if this energy which is no longer accounted for
        // in the surface heat balance is not accounted for elsewhere.  thus, these terms will dump
        // any reduced radiation into the air heat balance as an additional convective gain to avoid
        // any loss of energy in the overall heat balance.
        this->CurCoverEvapFac = 1.0 - (this->CurCoverSchedVal * this->CoverEvapFactor);
        this->CurCoverConvFac = 1.0 - (this->CurCoverSchedVal * this->CoverConvFactor);
        this->CurCoverSWRadFac = 1.0 - (this->CurCoverSchedVal * this->CoverSWRadFactor);
        this->CurCoverLWRadFac = 1.0 - (this->CurCoverSchedVal * this->CoverLWRadFactor);
    }

    void SwimmingPoolData::setupOutputVars()
    {
        SetupOutputVariable(
            "Indoor Pool Makeup Water Rate", OutputProcessor::Unit::m3_s, this->MakeUpWaterVolFlowRate, "System", "Average", this->Name);
        SetupOutputVariable("Indoor Pool Makeup Water Volume",
                            OutputProcessor::Unit::m3,
                            this->MakeUpWaterVol,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "MainsWater",
                            "Heating",
                            _,
                            "System");
        SetupOutputVariable(
            "Indoor Pool Makeup Water Temperature", OutputProcessor::Unit::C, this->CurMakeupWaterTemp, "System", "Average", this->Name);
        SetupOutputVariable("Indoor Pool Water Temperature", OutputProcessor::Unit::C, this->PoolWaterTemp, "System", "Average", this->Name);
        SetupOutputVariable("Indoor Pool Inlet Water Temperature", OutputProcessor::Unit::C, this->WaterInletTemp, "System", "Average", this->Name);
        SetupOutputVariable(
            "Indoor Pool Inlet Water Mass Flow Rate", OutputProcessor::Unit::kg_s, this->WaterMassFlowRate, "System", "Average", this->Name);
        SetupOutputVariable(
            "Indoor Pool Miscellaneous Equipment Power", OutputProcessor::Unit::W, this->MiscEquipPower, "System", "Average", this->Name);
        SetupOutputVariable(
            "Indoor Pool Miscellaneous Equipment Energy", OutputProcessor::Unit::J, this->MiscEquipEnergy, "System", "Sum", this->Name);
        SetupOutputVariable("Indoor Pool Water Heating Rate", OutputProcessor::Unit::W, this->HeatPower, "System", "Average", this->Name);
        SetupOutputVariable("Indoor Pool Water Heating Energy",
                            OutputProcessor::Unit::J,
                            this->HeatEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "HEATINGCOILS",
                            _,
                            "System");
        SetupOutputVariable(
            "Indoor Pool Radiant to Convection by Cover", OutputProcessor::Unit::W, this->RadConvertToConvect, "System", "Average", this->Name);
        SetupOutputVariable("Indoor Pool People Heat Gain", OutputProcessor::Unit::W, this->PeopleHeatGain, "System", "Average", this->Name);
        SetupOutputVariable(
            "Indoor Pool Current Activity Factor", OutputProcessor::Unit::None, this->CurActivityFactor, "System", "Average", this->Name);
        SetupOutputVariable("Indoor Pool Current Cover Factor", OutputProcessor::Unit::None, this->CurCoverSchedVal, "System", "Average", this->Name);
        SetupOutputVariable(
            "Indoor Pool Evaporative Heat Loss Rate", OutputProcessor::Unit::W, this->EvapHeatLossRate, "System", "Average", this->Name);
        SetupOutputVariable("Indoor Pool Evaporative Heat Loss Energy", OutputProcessor::Unit::J, this->EvapEnergyLoss, "System", "Sum", this->Name);
        SetupOutputVariable("Indoor Pool Saturation Pressure at Pool Temperature",
                            OutputProcessor::Unit::Pa,
                            this->SatPressPoolWaterTemp,
                            "System",
                            "Average",
                            this->Name);
        SetupOutputVariable("Indoor Pool Partial Pressure of Water Vapor in Air",
                            OutputProcessor::Unit::Pa,
                            this->PartPressZoneAirTemp,
                            "System",
                            "Average",
                            this->Name);
        SetupOutputVariable(
            "Indoor Pool Current Cover Evaporation Factor", OutputProcessor::Unit::None, this->CurCoverEvapFac, "System", "Average", this->Name);
        SetupOutputVariable(
            "Indoor Pool Current Cover Convective Factor", OutputProcessor::Unit::None, this->CurCoverConvFac, "System", "Average", this->Name);
        SetupOutputVariable(
            "Indoor Pool Current Cover SW Radiation Factor", OutputProcessor::Unit::None, this->CurCoverSWRadFac, "System", "Average", this->Name);
        SetupOutputVariable(
            "Indoor Pool Current Cover LW Radiation Factor", OutputProcessor::Unit::None, this->CurCoverLWRadFac, "System", "Average", this->Name);
    }

    void SwimmingPoolData::initSwimmingPoolPlantLoopIndex(BranchInputManagerData &dataBranchInputManager)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   June 2017

        bool errFlag;
        static std::string const RoutineName("InitSwimmingPoolPlantLoopIndex");

        if (MyPlantScanFlagPool && allocated(DataPlant::PlantLoop)) {
            errFlag = false;
            if (this->WaterInletNode > 0) {
                PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
                                                        this->Name,
                                                        DataPlant::TypeOf_SwimmingPool_Indoor,
                                                        this->HWLoopNum,
                                                        this->HWLoopSide,
                                                        this->HWBranchNum,
                                                        this->HWCompNum,
                                                        errFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        this->WaterInletNode,
                                                        _);
                if (errFlag) {
                    ShowFatalError(RoutineName + ": Program terminated due to previous condition(s).");
                }
            }
            this->MyPlantScanFlagPool = false;
        } else if (this->MyPlantScanFlagPool && !DataGlobals::AnyPlantInModel) {
            this->MyPlantScanFlagPool = false;
        }
    }

    void SwimmingPoolData::initSwimmingPoolPlantNodeFlow(bool const MyPlantScanFlagPool // logical flag true when plant index has not yet been set
    )
    {

        if (!MyPlantScanFlagPool) {
            if (this->WaterInletNode > 0) {
                PlantUtilities::InitComponentNodes(0.0,
                                                   this->WaterMassFlowRateMax,
                                                   this->WaterInletNode,
                                                   this->WaterOutletNode,
                                                   this->HWLoopNum,
                                                   this->HWLoopSide,
                                                   this->HWBranchNum,
                                                   this->HWCompNum);
                PlantUtilities::RegisterPlantCompDesignFlow(this->WaterInletNode, this->WaterVolFlowMax);
            }
        }
    }

    void SwimmingPoolData::calculate()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand, Ho-Sung Kim
        //       DATE WRITTEN   October 2014

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the components making up the Indoor Swimming Pool model.

        // METHODOLOGY EMPLOYED:
        // The swimming pool is modeled as a SURFACE to get access to all of the existing
        // surface related algorithms.  This subroutine mainly models the components of the
        // swimming pool so that information can be used in a standard surface heat balance.
        // The pool is assumed to be located at the inside surface face with a possible cover
        // affecting the heat balance.  The pool model takes the form of an equation solving
        // for the inside surface temperature which is assumed to be the same as the pool
        // water temperature.
        // Standard Heat Balance Equation:
        //		TempSurfInTmp( SurfNum ) = ( CTFConstInPart( SurfNum ) + QRadThermInAbs( SurfNum ) + QRadSWInAbs( SurfNum ) + HConvIn( SurfNum
        //)
        //* RefAirTemp( SurfNum ) + NetLWRadToSurf( SurfNum ) + Construct( ConstrNum ).CTFSourceIn( 0 ) * QsrcHist( 1, SurfNum ) + QHTRadSysSurf(
        // SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) + IterDampConst * TempInsOld(
        // SurfNum ) + Construct( ConstrNum ).CTFCross( 0 ) * TH11 ) / ( Construct( ConstrNum ).CTFInside( 0 ) + HConvIn( SurfNum ) + IterDampConst );
        //// Constant part of conduction eq (history terms) | LW radiation from internal sources | SW radiation from internal sources | Convection
        // from surface to zone air | Net radiant exchange with other zone surfaces | Heat source/sink term for radiant systems | (if there is one
        // present) | Radiant flux from high temp radiant heater | Radiant flux from a hot water baseboard heater | Radiant flux from a steam
        // baseboard  heater | Radiant flux from an electric baseboard heater | Iterative damping term (for stability) | Current conduction from | the
        // outside  surface | Coefficient for conduction (current time) | Convection and damping term
        // That equation is modified to include pool specific terms and removes the IterDampConst
        // term which is for iterations within the inside surface heat balance.  Then, the resulting
        // equation is solved for the plant loop mass flow rate.  It also assigns the appropriate
        // terms for use in the actual heat balance routine.

        // REFERENCES:
        //  1. ASHRAE (2011). 2011 ASHRAE Handbook - HVAC Applications. Atlanta: American Society of Heating,
        //     Refrigerating and Air-Conditioning Engineers, Inc., p.5.6-5.9.
        //  2. Janis, R. and W. Tao (2005). Mechanical and Electrical Systems in Buildings. 3rd ed. Upper
        //     Saddle River, NJ: Pearson Education, Inc., p.246.
        //  3. Kittler, R. (1989). Indoor Natatorium Design and Energy Recycling. ASHRAE Transactions 95(1), p.521-526.
        //  4. Smith, C., R. Jones, and G. Lof (1993). Energy Requirements and Potential Savings for Heated
        //     Indoor Swimming Pools. ASHRAE Transactions 99(2), p.864-874.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcSwimmingPool");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 EvapRate;  // evaporation rate for pool in kg/s
        Real64 CpDeltaTi; // inverse of specific heat of water times the plant loop temperature difference

        // initialize local variables
        int SurfNum = this->SurfacePtr;                    // surface number of floor that is the pool
        int ZoneNum = DataSurfaces::Surface(SurfNum).Zone; // index to zone array

        // Convection coefficient calculation
        Real64 HConvIn = 0.22 * std::pow(std::abs(this->PoolWaterTemp - DataHeatBalFanSys::MAT(ZoneNum)), 1.0 / 3.0) *
                         this->CurCoverConvFac; // convection coefficient for pool
        calcSwimmingPoolEvap(EvapRate, SurfNum, DataHeatBalFanSys::MAT(ZoneNum), DataHeatBalFanSys::ZoneAirHumRat(ZoneNum));
        this->MakeUpWaterMassFlowRate = EvapRate;
        Real64 EvapEnergyLossPerArea = -EvapRate *
                                       Psychrometrics::PsyHfgAirFnWTdb(DataHeatBalFanSys::ZoneAirHumRat(ZoneNum), DataHeatBalFanSys::MAT(ZoneNum)) /
                                       DataSurfaces::Surface(SurfNum).Area; // energy effect of evaporation rate per unit area in W/m2
        this->EvapHeatLossRate = EvapEnergyLossPerArea * DataSurfaces::Surface(SurfNum).Area;
        // LW and SW radiation term modification: any "excess" radiation blocked by the cover gets convected
        // to the air directly and added to the zone air heat balance
        Real64 LWsum =
            (DataHeatBalance::QRadThermInAbs(SurfNum) + DataHeatBalSurface::NetLWRadToSurf(SurfNum) + DataHeatBalFanSys::QHTRadSysSurf(SurfNum) +
             DataHeatBalFanSys::QHWBaseboardSurf(SurfNum) + DataHeatBalFanSys::QSteamBaseboardSurf(SurfNum) +
             DataHeatBalFanSys::QElecBaseboardSurf(SurfNum)); // summation of all long-wavelenth radiation going to surface
        Real64 LWtotal = this->CurCoverLWRadFac * LWsum;      // total flux from long-wavelength radiation to surface
        Real64 SWtotal = this->CurCoverSWRadFac * DataHeatBalSurface::QRadSWInAbs(SurfNum); // total flux from short-wavelength radiation to surface
        this->RadConvertToConvect =
            ((1.0 - this->CurCoverLWRadFac) * LWsum) + ((1.0 - this->CurCoverSWRadFac) * DataHeatBalSurface::QRadSWInAbs(SurfNum));

        // Heat gain from people (assumed to be all convective to pool water)
        Real64 PeopleGain =
            this->PeopleHeatGain / DataSurfaces::Surface(SurfNum).Area; // heat gain from people in pool (assumed to be all convective)

        // Get an estimate of the pool water specific heat
        Real64 Cp =
            FluidProperties::GetSpecificHeatGlycol("WATER", this->PoolWaterTemp, this->GlycolIndex, RoutineName); // specific heat of pool water

        Real64 TH22 =
            DataHeatBalSurface::TH(2, 2, SurfNum); // inside surface temperature at the previous time step equals the old pool water temperature
        Real64 TInSurf =
            this->CurSetPtTemp; // Setpoint temperature for pool which is also the goal temperature and also the inside surface face temperature
        Real64 Tmuw = this->CurMakeupWaterTemp;                                // Inlet makeup water temperature
        Real64 TLoopInletTemp = DataLoopNode::Node(this->WaterInletNode).Temp; // Inlet water temperature from the plant loop
        this->WaterInletTemp = TLoopInletTemp;

        if (TLoopInletTemp <= TInSurf) {
            CpDeltaTi = 0.0;
        } else {
            CpDeltaTi = 1.0 / (Cp * (TInSurf - TLoopInletTemp));
        }
        // Now calculate the requested mass flow rate from the plant loop to achieve the proper pool temperature
        // old equation using surface heat balance form: MassFlowRate = CpDeltaTi * ( CondTerms + ConvTerm + SWtotal + LWtotal + PeopleGain +
        // PoolMassTerm + MUWTerm + EvapEnergyLossPerArea );
        Real64 MassFlowRate = (this->WaterMass / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour)) *
                              ((TInSurf - TH22) / (TLoopInletTemp - TInSurf)); // Target mass flow rate to achieve the proper setpoint temperature
        if (MassFlowRate > this->WaterMassFlowRateMax) {
            MassFlowRate = this->WaterMassFlowRateMax;
        } else if (MassFlowRate < 0.0) {
            MassFlowRate = 0.0;
        }
        PlantUtilities::SetComponentFlowRate(
            MassFlowRate, this->WaterInletNode, this->WaterOutletNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchNum, this->HWCompNum);
        this->WaterMassFlowRate = MassFlowRate;

        // We now have a flow rate so we can assemble the terms needed for the surface heat balance that is solved for the inside face temperature
        DataHeatBalFanSys::QPoolSurfNumerator(SurfNum) =
            SWtotal + LWtotal + PeopleGain + EvapEnergyLossPerArea + HConvIn * DataHeatBalFanSys::MAT(ZoneNum) +
            (EvapRate * Tmuw + MassFlowRate * TLoopInletTemp + (this->WaterMass * TH22 / DataGlobals::TimeStepZoneSec)) * Cp /
                DataSurfaces::Surface(SurfNum).Area;
        DataHeatBalFanSys::PoolHeatTransCoefs(SurfNum) =
            HConvIn + (EvapRate + MassFlowRate + (this->WaterMass / DataGlobals::TimeStepZoneSec)) * Cp / DataSurfaces::Surface(SurfNum).Area;

        // Finally take care of the latent and convective gains resulting from the pool
        DataHeatBalFanSys::SumConvPool(ZoneNum) += this->RadConvertToConvect;
        DataHeatBalFanSys::SumLatentPool(ZoneNum) +=
            EvapRate * Psychrometrics::PsyHfgAirFnWTdb(DataHeatBalFanSys::ZoneAirHumRat(ZoneNum), DataHeatBalFanSys::MAT(ZoneNum));
    }

    void SwimmingPoolData::calcSwimmingPoolEvap(Real64 &EvapRate,   // evaporation rate of pool
                                                int const SurfNum,  // surface index
                                                Real64 const MAT,   // mean air temperature
                                                Real64 const HumRat // zone air humidity ratio
    )
    {
        static std::string const RoutineName("CalcSwimmingPoolEvap");
        Real64 const CFinHg(0.00029613); // Multiple pressure in Pa by this constant to get inches of Hg

        // Evaporation calculation:
        // Evaporation Rate (lb/h) = 0.1 * Area (ft2) * Activity Factor * (Psat,pool - Ppar,air) (in Hg)
        // So evaporation rate, area, and pressures have to be converted to standard E+ units (kg/s, m2, and Pa, respectively)
        // Evaporation Rate per Area = Evaporation Rate * Heat of Vaporization / Area of Surface

        Real64 PSatPool = Psychrometrics::PsyPsatFnTemp(this->PoolWaterTemp, RoutineName);
        Real64 PParAir = Psychrometrics::PsyPsatFnTemp(MAT, RoutineName) * Psychrometrics::PsyRhFnTdbWPb(MAT, HumRat, DataEnvironment::OutBaroPress);
        if (PSatPool < PParAir) PSatPool = PParAir;
        this->SatPressPoolWaterTemp = PSatPool;
        this->PartPressZoneAirTemp = PParAir;
        EvapRate = (0.1 * (DataSurfaces::Surface(SurfNum).Area / DataConversions::CFA) * this->CurActivityFactor * ((PSatPool - PParAir) * CFinHg)) *
                   DataConversions::CFMF * this->CurCoverEvapFac;
    }

    void SwimmingPoolData::update()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand, Ho-Sung Kim
        //       DATE WRITTEN   October 2014

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine does any updating that needs to be done for the swimming pool model.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("UpdateSwimmingPool");

        int SurfNum = this->SurfacePtr; // surface number/pointer

        if (this->LastSysTimeElapsed(SurfNum) == DataHVACGlobals::SysTimeElapsed) {
            // Still iterating or reducing system time step, so subtract old values which were
            // not valid
            this->QPoolSrcAvg(SurfNum) -= this->LastQPoolSrc(SurfNum) * this->LastTimeStepSys(SurfNum) / DataGlobals::TimeStepZone;
            this->HeatTransCoefsAvg(SurfNum) -= this->LastHeatTransCoefs(SurfNum) * this->LastTimeStepSys(SurfNum) / DataGlobals::TimeStepZone;
        }

        // Update the running average and the "last" values with the current values of the appropriate variables
        this->QPoolSrcAvg(SurfNum) += DataHeatBalFanSys::QPoolSurfNumerator(SurfNum) * DataHVACGlobals::TimeStepSys / DataGlobals::TimeStepZone;
        this->HeatTransCoefsAvg(SurfNum) += DataHeatBalFanSys::PoolHeatTransCoefs(SurfNum) * DataHVACGlobals::TimeStepSys / DataGlobals::TimeStepZone;

        this->LastQPoolSrc(SurfNum) = DataHeatBalFanSys::QPoolSurfNumerator(SurfNum);
        this->LastHeatTransCoefs(SurfNum) = DataHeatBalFanSys::PoolHeatTransCoefs(SurfNum);
        this->LastSysTimeElapsed(SurfNum) = DataHVACGlobals::SysTimeElapsed;
        this->LastTimeStepSys(SurfNum) = DataHVACGlobals::TimeStepSys;

        int WaterInletNode = this->WaterInletNode;   // inlet node number
        int WaterOutletNode = this->WaterOutletNode; // outlet node number
        PlantUtilities::SafeCopyPlantNode(WaterInletNode, WaterOutletNode);

        Real64 WaterMassFlow = DataLoopNode::Node(WaterInletNode).MassFlowRate; // water mass flow rate
        if (WaterMassFlow > 0.0) DataLoopNode::Node(WaterOutletNode).Temp = this->PoolWaterTemp;
    }

    void UpdatePoolSourceValAvg(bool &SwimmingPoolOn) // .TRUE. if the swimming pool "runs" this zone time step
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   October 2014

        // PURPOSE OF THIS SUBROUTINE:
        // To transfer the average value of the pool heat balance term over the entire zone time step back to the heat balance routines so that the
        // heat balance algorithms can simulate one last time with the average source to maintain some reasonable amount of continuity and energy
        // balance in the temperature and flux histories.

        // METHODOLOGY EMPLOYED:
        // All of the record keeping for the average term is done in the Update routine so the only other thing that this subroutine does is check to
        // see if the system was even on.  If any average term is non-zero, then one or more of the swimming pools was running.  Method borrowed from
        // radiant systems.

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CloseEnough(0.01); // Some arbitrarily small value to avoid zeros and numbers that are almost the same

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // DO loop counter for surface index

        SwimmingPoolOn = false;

        // If this was never allocated, then there are no radiant systems in this input file (just RETURN)

        for (int PoolNum = 1; PoolNum <= NumSwimmingPools; ++PoolNum) {
            if (!allocated(Pool(PoolNum).QPoolSrcAvg)) return;

            // If it was allocated, then we have to check to see if this was running at all
            for (SurfNum = 1; SurfNum <= DataSurfaces::TotSurfaces; ++SurfNum) {
                if (Pool(PoolNum).QPoolSrcAvg(SurfNum) != 0.0) {
                    SwimmingPoolOn = true;
                    break; // DO loop
                }
            }

            DataHeatBalFanSys::QPoolSurfNumerator = Pool(PoolNum).QPoolSrcAvg;
            DataHeatBalFanSys::PoolHeatTransCoefs = Pool(PoolNum).HeatTransCoefsAvg;
        }

        // For interzone surfaces, modQPoolSrcAvg was only updated for the "active" side.  The active side
        // would have a non-zero value at this point.  If the numbers differ, then we have to manually update.
        for (SurfNum = 1; SurfNum <= DataSurfaces::TotSurfaces; ++SurfNum) {
            if (DataSurfaces::Surface(SurfNum).ExtBoundCond > 0 && DataSurfaces::Surface(SurfNum).ExtBoundCond != SurfNum) {
                if (std::abs(DataHeatBalFanSys::QPoolSurfNumerator(SurfNum) -
                             DataHeatBalFanSys::QPoolSurfNumerator(DataSurfaces::Surface(SurfNum).ExtBoundCond)) > CloseEnough) { // numbers differ
                    if (std::abs(DataHeatBalFanSys::QPoolSurfNumerator(SurfNum)) >
                        std::abs(DataHeatBalFanSys::QPoolSurfNumerator(DataSurfaces::Surface(SurfNum).ExtBoundCond))) {
                        DataHeatBalFanSys::QPoolSurfNumerator(DataSurfaces::Surface(SurfNum).ExtBoundCond) =
                            DataHeatBalFanSys::QPoolSurfNumerator(SurfNum);
                    } else {
                        DataHeatBalFanSys::QPoolSurfNumerator(SurfNum) =
                            DataHeatBalFanSys::QPoolSurfNumerator(DataSurfaces::Surface(SurfNum).ExtBoundCond);
                    }
                }
            }
        }
        // For interzone surfaces, PoolHeatTransCoefs was only updated for the "active" side.  The active side
        // would have a non-zero value at this point.  If the numbers differ, then we have to manually update.
        for (SurfNum = 1; SurfNum <= DataSurfaces::TotSurfaces; ++SurfNum) {
            if (DataSurfaces::Surface(SurfNum).ExtBoundCond > 0 && DataSurfaces::Surface(SurfNum).ExtBoundCond != SurfNum) {
                if (std::abs(DataHeatBalFanSys::PoolHeatTransCoefs(SurfNum) -
                             DataHeatBalFanSys::PoolHeatTransCoefs(DataSurfaces::Surface(SurfNum).ExtBoundCond)) > CloseEnough) { // numbers differ
                    if (std::abs(DataHeatBalFanSys::PoolHeatTransCoefs(SurfNum)) >
                        std::abs(DataHeatBalFanSys::PoolHeatTransCoefs(DataSurfaces::Surface(SurfNum).ExtBoundCond))) {
                        DataHeatBalFanSys::PoolHeatTransCoefs(DataSurfaces::Surface(SurfNum).ExtBoundCond) =
                            DataHeatBalFanSys::PoolHeatTransCoefs(SurfNum);
                    } else {
                        DataHeatBalFanSys::PoolHeatTransCoefs(SurfNum) =
                            DataHeatBalFanSys::PoolHeatTransCoefs(DataSurfaces::Surface(SurfNum).ExtBoundCond);
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
        // The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector and should be updated accordingly.

        Real64 SumHATsurf = 0.0; // Return value

        for (int SurfNum = DataHeatBalance::Zone(ZoneNum).SurfaceFirst; SurfNum <= DataHeatBalance::Zone(ZoneNum).SurfaceLast; ++SurfNum) {
            if (!DataSurfaces::Surface(SurfNum).HeatTransSurf) continue; // Skip non-heat transfer surfaces

            Real64 Area = DataSurfaces::Surface(SurfNum).Area; // Effective surface area

            if (DataSurfaces::Surface(SurfNum).Class == DataSurfaces::SurfaceClass_Window) {
                if (DataSurfaces::SurfaceWindow(SurfNum).ShadingFlag == DataSurfaces::IntShadeOn ||
                    DataSurfaces::SurfaceWindow(SurfNum).ShadingFlag == DataSurfaces::IntBlindOn) {
                    // The area is the shade or blind are = sum of the glazing area and the divider area (which is zero if no divider)
                    Area += DataSurfaces::SurfaceWindow(SurfNum).DividerArea;
                }

                if (DataSurfaces::SurfaceWindow(SurfNum).FrameArea > 0.0) {
                    // Window frame contribution
                    SumHATsurf += DataHeatBalance::HConvIn(SurfNum) * DataSurfaces::SurfaceWindow(SurfNum).FrameArea *
                                  (1.0 + DataSurfaces::SurfaceWindow(SurfNum).ProjCorrFrIn) * DataSurfaces::SurfaceWindow(SurfNum).FrameTempSurfIn;
                }

                if (DataSurfaces::SurfaceWindow(SurfNum).DividerArea > 0.0 &&
                    DataSurfaces::SurfaceWindow(SurfNum).ShadingFlag != DataSurfaces::IntShadeOn &&
                    DataSurfaces::SurfaceWindow(SurfNum).ShadingFlag != DataSurfaces::IntBlindOn) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    SumHATsurf += DataHeatBalance::HConvIn(SurfNum) * DataSurfaces::SurfaceWindow(SurfNum).DividerArea *
                                  (1.0 + 2.0 * DataSurfaces::SurfaceWindow(SurfNum).ProjCorrDivIn) *
                                  DataSurfaces::SurfaceWindow(SurfNum).DividerTempSurfIn;
                }
            }

            SumHATsurf += DataHeatBalance::HConvIn(SurfNum) * Area * DataHeatBalSurface::TempSurfInTmp(SurfNum);
        }

        return SumHATsurf;
    }

    void ReportSwimmingPool()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand, Ho-Sung Kim
        //       DATE WRITTEN   October 2014

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simply produces output for the swimming pool model.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("ReportSwimmingPool");
        Real64 const MinDensity = 1.0; // to avoid a divide by zero

        for (int PoolNum = 1; PoolNum <= NumSwimmingPools; ++PoolNum) {

            int SurfNum = Pool(PoolNum).SurfacePtr; // surface number index

            // First transfer the surface inside temperature data to the current pool water temperature
            Pool(PoolNum).PoolWaterTemp = DataHeatBalSurface::TH(2, 1, SurfNum);

            // Next calculate the amount of heating done by the plant loop
            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
                "WATER", Pool(PoolNum).PoolWaterTemp, Pool(PoolNum).GlycolIndex, RoutineName); // specific heat of water
            Pool(PoolNum).HeatPower = Pool(PoolNum).WaterMassFlowRate * Cp * (Pool(PoolNum).WaterInletTemp - Pool(PoolNum).PoolWaterTemp);

            // Now the power consumption of miscellaneous equipment
            Real64 Density =
                FluidProperties::GetDensityGlycol("WATER", Pool(PoolNum).PoolWaterTemp, Pool(PoolNum).GlycolIndex, RoutineName); // density of water
            if (Density > MinDensity) {
                Pool(PoolNum).MiscEquipPower = Pool(PoolNum).MiscPowerFactor * Pool(PoolNum).WaterMassFlowRate / Density;
            } else {
                Pool(PoolNum).MiscEquipPower = 0.0;
            }

            // Also the radiant exchange converted to convection by the pool cover
            Pool(PoolNum).RadConvertToConvectRep = Pool(PoolNum).RadConvertToConvect * DataSurfaces::Surface(SurfNum).Area;

            // Finally calculate the summed up report variables
            Pool(PoolNum).MiscEquipEnergy = Pool(PoolNum).MiscEquipPower * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            Pool(PoolNum).HeatEnergy = Pool(PoolNum).HeatPower * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            Pool(PoolNum).MakeUpWaterMass = Pool(PoolNum).MakeUpWaterMassFlowRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            Pool(PoolNum).EvapEnergyLoss = Pool(PoolNum).EvapHeatLossRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

            Pool(PoolNum).MakeUpWaterVolFlowRate = MakeUpWaterVolFlowFunct(Pool(PoolNum).MakeUpWaterMassFlowRate, Density);
            Pool(PoolNum).MakeUpWaterVol = MakeUpWaterVolFunct(Pool(PoolNum).MakeUpWaterMass, Density);
        }
    }

    Real64 MakeUpWaterVolFlowFunct(Real64 MakeUpWaterMassFlowRate, Real64 Density)
    {
        Real64 MakeUpWaterVolumeFlow;
        MakeUpWaterVolumeFlow = MakeUpWaterMassFlowRate / Density;
        return MakeUpWaterVolumeFlow;
    }

    Real64 MakeUpWaterVolFunct(Real64 MakeUpWaterMass, Real64 Density)
    {
        Real64 MakeUpWaterVolume;
        MakeUpWaterVolume = MakeUpWaterMass / Density;
        return MakeUpWaterVolume;
    }

} // namespace SwimmingPool

} // namespace EnergyPlus
