// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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
#include <cmath>
#include <fstream>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// JSON Headers
#include <nlohmann/json.hpp>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GroundHeatExchangers.hh>
#include <EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::GroundHeatExchangers {
// MODULE INFORMATION:
//       AUTHOR         Arun Murugappan, Dan Fisher
//       DATE WRITTEN   September 2000
//       MODIFIED       B. Griffith, Sept 2010,plant upgrades
//                      Matt Mitchell, February 2015. Added Slinky GHX.
//                                                    Moved models to object-oriented design.
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// The module contains the data structures and routines to simulate the
// operation of vertical closed-loop ground heat exchangers (GLHE) typically
// used in low temperature geothermal heat pump systems.

// METHODOLOGY EMPLOYED:
// The borehole and fluid temperatures are calculated from the response to
// the current heat transfer rate and the response to the history of past
// applied heat pulses. The response to each pulse is calculated from a non-
// dimensionalized response function, or G-function, that is specific to the
// given borehole field arrangement, depth and spacing. The data defining
// this function is read from input.
// The heat pulse histories need to be recorded over an extended period (months).
// To aid computational efficiency past pulses are continuously aggregated into
// equivalent heat pulses of longer duration, as each pulse becomes less recent.

// REFERENCES:
// Eskilson, P. 'Thermal Analysis of Heat Extraction Boreholes' Ph.D. Thesis:
//   Dept. of Mathematical Physics, University of Lund, Sweden, June 1987.
// Yavuzturk, C., J.D. Spitler. 1999. 'A Short Time Step Response Factor Model
//   for Vertical Ground Loop Heat Exchangers. ASHRAE Transactions. 105(2): 475-485.
// Xiong, Z., D.E. Fisher, J.D. Spitler. 2015. 'Development and Validation of a Slinky
//   Ground Heat Exchanger.' Applied Energy. Vol 114, 57-69.

// Using/Aliasing
using namespace DataLoopNode;

using namespace GroundTemperatureManager;

// MODULE PARAMETER DEFINITIONS
constexpr Real64 hrsPerMonth(730.0); // Number of hours in month
constexpr Real64 maxTSinHr(60);      // Max number of time step in a hour

//******************************************************************************

GLHESlinky::GLHESlinky(EnergyPlusData &state, std::string const &objName, nlohmann::json const &j)
{
    // Check for duplicates
    for (auto &existingObj : state.dataGroundHeatExchanger->singleBoreholesVector) {
        if (objName == existingObj->name) {
            ShowFatalError(state, "Invalid input for " + this->moduleName + " object: Duplicate name found: " + existingObj->name);
        }
    }

    bool errorsFound = false;

    this->name = objName;

    std::string inletNodeName = UtilityRoutines::MakeUPPERCase(j["inlet_node_name"]);
    std::string outletNodeName = UtilityRoutines::MakeUPPERCase(j["outlet_node_name"]);

    // get inlet node num
    this->inletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                             inletNodeName,
                                                             errorsFound,
                                                             this->moduleName,
                                                             this->name,
                                                             DataLoopNode::NodeFluidType::Water,
                                                             DataLoopNode::NodeConnectionType::Inlet,
                                                             NodeInputManager::compFluidStream::Primary,
                                                             ObjectIsNotParent);

    // get outlet node num
    this->outletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                              outletNodeName,
                                                              errorsFound,
                                                              this->moduleName,
                                                              this->name,
                                                              DataLoopNode::NodeFluidType::Water,
                                                              DataLoopNode::NodeConnectionType::Outlet,
                                                              NodeInputManager::compFluidStream::Primary,
                                                              ObjectIsNotParent);

    this->available = true;
    this->on = true;

    BranchNodeConnections::TestCompSet(state, this->moduleName, this->name, inletNodeName, outletNodeName, "Condenser Water Nodes");

    // load data
    this->designFlow = j["design_flow_rate"];
    PlantUtilities::RegisterPlantCompDesignFlow(state, this->inletNodeNum, this->designFlow);

    this->soil.k = j["soil_thermal_conductivity"];
    this->soil.rho = j["soil_density"];
    this->soil.cp = j["soil_specific_heat"];
    this->soil.rhoCp = this->soil.rho * this->soil.cp;
    this->pipe.k = j["pipe_thermal_conductivity"];
    this->pipe.rho = j["pipe_density"];
    this->pipe.cp = j["pipe_specific_heat"];
    this->pipe.outDia = j["pipe_outer_diameter"];
    this->pipe.outRadius = this->pipe.outDia / 2.0;
    this->pipe.thickness = j["pipe_thickness"];

    std::string const hxConfig = UtilityRoutines::MakeUPPERCase(j["heat_exchanger_configuration"]);
    if (UtilityRoutines::SameString(hxConfig, "VERTICAL")) {
        this->verticalConfig = true;
    } else if (UtilityRoutines::SameString(hxConfig, "HORIZONTAL")) {
        this->verticalConfig = false;
    }

    this->coilDiameter = j["coil_diameter"];
    this->coilPitch = j["coil_pitch"];
    this->trenchDepth = j["trench_depth"];
    this->trenchLength = j["trench_length"];
    this->numTrenches = j["number_of_trenches"];
    this->trenchSpacing = j["horizontal_spacing_between_pipes"];
    this->maxSimYears = j["maximum_length_of_simulation"];

    // Need to add a response factor object for the slinky model
    std::shared_ptr<GLHEResponseFactors> thisRF(new GLHEResponseFactors);
    thisRF->name = "Response Factor Object Auto Generated No: " + fmt::to_string(state.dataGroundHeatExchanger->numAutoGeneratedResponseFactors + 1);
    this->myRespFactors = thisRF;
    state.dataGroundHeatExchanger->responseFactorsVector.push_back(thisRF);

    // Number of coils
    this->numCoils = static_cast<int>(this->trenchLength / this->coilPitch);

    // Total tube length
    this->totalTubeLength = DataGlobalConstants::Pi * this->coilDiameter * this->trenchLength * this->numTrenches / this->coilPitch;

    // Get g function data
    this->SubAGG = 15;
    this->AGG = 192;

    // Average coil depth
    if (this->verticalConfig) {
        // Vertical configuration
        if (this->trenchDepth - this->coilDiameter < 0.0) {
            // Error: part of the coil is above ground
            ShowSevereError(state, this->moduleName + "=\"" + this->name + "\", invalid value in field.");
            ShowContinueError(state, format("...{}=[{:.3R}].", "Trench Depth", this->trenchDepth));
            ShowContinueError(state, format("...{}=[{:.3R}].", "Coil Depth", this->coilDepth));
            ShowContinueError(state, "...Part of coil will be above ground.");
            errorsFound = true;

        } else {
            // Entire coil is below ground
            this->coilDepth = this->trenchDepth - (this->coilDiameter / 2.0);
        }

    } else {
        // Horizontal configuration
        this->coilDepth = this->trenchDepth;
    }

    // Thermal diffusivity of the ground
    this->soil.diffusivity = this->soil.k / this->soil.rhoCp;

    state.dataGroundHeatExchanger->prevTimeSteps.allocate(static_cast<int>((this->SubAGG + 1) * maxTSinHr + 1));
    state.dataGroundHeatExchanger->prevTimeSteps = 0.0;

    if (this->pipe.thickness >= this->pipe.outDia / 2.0) {
        ShowSevereError(state, this->moduleName + "=\"" + this->name + "\", invalid value in field.");
        ShowContinueError(state, format("...{}=[{:.3R}].", "Pipe Thickness", this->pipe.thickness));
        ShowContinueError(state, format("...{}=[{:.3R}].", "Pipe Outer Diameter", this->pipe.outDia));
        ShowContinueError(state, "...Radius will be <=0.");
        errorsFound = true;
    }

    // Initialize ground temperature model and get pointer reference
    std::string const gtmType = UtilityRoutines::MakeUPPERCase(j["undisturbed_ground_temperature_model_type"]);
    std::string const gtmName = UtilityRoutines::MakeUPPERCase(j["undisturbed_ground_temperature_model_name"]);
    this->groundTempModel = GetGroundTempModelAndInit(state, gtmType, gtmName);
    if (this->groundTempModel) {
        errorsFound = this->groundTempModel->errorsFound;
    }

    // Check for Errors
    if (errorsFound) {
        ShowFatalError(state, "Errors found in processing input for " + this->moduleName);
    }
}

//******************************************************************************

GLHEVert::GLHEVert(EnergyPlusData &state, std::string const &objName, nlohmann::json const &j)
{
    // Check for duplicates
    for (auto &existingObj : state.dataGroundHeatExchanger->singleBoreholesVector) {
        if (objName == existingObj->name) {
            ShowFatalError(state, "Invalid input for " + this->moduleName + " object: Duplicate name found: " + existingObj->name);
        }
    }

    bool errorsFound = false;

    this->name = objName;

    // get inlet node num
    std::string const inletNodeName = UtilityRoutines::MakeUPPERCase(j["inlet_node_name"]);
    this->inletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                             inletNodeName,
                                                             errorsFound,
                                                             this->moduleName,
                                                             objName,
                                                             DataLoopNode::NodeFluidType::Water,
                                                             DataLoopNode::NodeConnectionType::Inlet,
                                                             NodeInputManager::compFluidStream::Primary,
                                                             ObjectIsNotParent);

    // get outlet node num
    std::string const outletNodeName = UtilityRoutines::MakeUPPERCase(j["outlet_node_name"]);
    this->outletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                              outletNodeName,
                                                              errorsFound,
                                                              this->moduleName,
                                                              objName,
                                                              DataLoopNode::NodeFluidType::Water,
                                                              DataLoopNode::NodeConnectionType::Outlet,
                                                              NodeInputManager::compFluidStream::Primary,
                                                              ObjectIsNotParent);
    this->available = true;
    this->on = true;

    BranchNodeConnections::TestCompSet(state, this->moduleName, objName, inletNodeName, outletNodeName, "Condenser Water Nodes");

    this->designFlow = j["design_flow_rate"];
    PlantUtilities::RegisterPlantCompDesignFlow(state, this->inletNodeNum, this->designFlow);

    this->soil.k = j["ground_thermal_conductivity"];
    this->soil.rhoCp = j["ground_thermal_heat_capacity"];

    if (j.find("ghe_vertical_responsefactors_object_name") != j.end()) {
        // Response factors come from IDF object
        this->myRespFactors = GetResponseFactor(state, UtilityRoutines::MakeUPPERCase(j["ghe_vertical_responsefactors_object_name"]));
        this->gFunctionsExist = true;

        if (!this->myRespFactors) {
            errorsFound = true;
            ShowSevereError(state, "GroundHeatExchanger:ResponseFactors object not found.");
        }
    } else if (j.find("ghe_vertical_array_object_name") != j.end()) {
        // Response factors come from array object
        this->myRespFactors =
            BuildAndGetResponseFactorObjectFromArray(state, GetVertArray(state, UtilityRoutines::MakeUPPERCase(j["ghe_vertical_array_object_name"])));

        if (!this->myRespFactors) {
            errorsFound = true;
            ShowSevereError(state, "GroundHeatExchanger:Vertical:Array object not found.");
        }
    } else {
        if (j.find("vertical_well_locations") == j.end()) {
            // No ResponseFactors, GHEArray, or SingleBH object are referenced
            ShowSevereError(state, "No GHE:ResponseFactors, GHE:Vertical:Array, or GHE:Vertical:Single objects found");
            ShowFatalError(state, "Check references to these objects for GHE:System object: " + this->name);
        }

        auto const vars = j.at("vertical_well_locations");

        // Calculate response factors from individual boreholes
        std::vector<std::shared_ptr<GLHEVertSingle>> tempVectOfBHObjects;

        for (auto const &var : vars) {
            if (!var.at("ghe_vertical_single_object_name").empty()) {
                std::shared_ptr<GLHEVertSingle> tempBHptr =
                    GetSingleBH(state, UtilityRoutines::MakeUPPERCase(var.at("ghe_vertical_single_object_name")));
                if (tempBHptr) {
                    tempVectOfBHObjects.push_back(tempBHptr);
                } else {
                    errorsFound = true;
                    std::string const tmpName = var.at("ghe_vertical_single_object_name");
                    ShowSevereError(state, "Borehole= " + tmpName + " not found.");
                    break;
                }
            } else {
                break;
            }
        }

        this->myRespFactors = BuildAndGetResponseFactorsObjectFromSingleBHs(state, tempVectOfBHObjects);

        if (!this->myRespFactors) {
            errorsFound = true;
            ShowSevereError(state, "GroundHeatExchanger:Vertical:Single objects not found.");
        }
    }

    this->bhDiameter = this->myRespFactors->props->bhDiameter;
    this->bhRadius = this->bhDiameter / 2.0;
    this->bhLength = this->myRespFactors->props->bhLength;
    this->bhUTubeDist = this->myRespFactors->props->bhUTubeDist;

    // pull pipe and grout data up from response factor struct for simplicity
    this->pipe.outDia = this->myRespFactors->props->pipe.outDia;
    this->pipe.innerDia = this->myRespFactors->props->pipe.innerDia;
    this->pipe.outRadius = this->pipe.outDia / 2;
    this->pipe.innerRadius = this->pipe.innerDia / 2;
    this->pipe.thickness = this->myRespFactors->props->pipe.thickness;
    this->pipe.k = this->myRespFactors->props->pipe.k;
    this->pipe.rhoCp = this->myRespFactors->props->pipe.rhoCp;

    this->grout.k = this->myRespFactors->props->grout.k;
    this->grout.rhoCp = this->myRespFactors->props->grout.rhoCp;

    this->myRespFactors->gRefRatio = this->bhRadius / this->bhLength;

    // Number of simulation years from RunPeriod
    this->myRespFactors->maxSimYears = state.dataEnvrn->MaxNumberSimYears;

    // total tube length
    this->totalTubeLength = this->myRespFactors->numBoreholes * this->myRespFactors->props->bhLength;

    // ground thermal diffusivity
    this->soil.diffusivity = this->soil.k / this->soil.rhoCp;

    // multipole method constants
    this->theta_1 = this->bhUTubeDist / (2 * this->bhRadius);
    this->theta_2 = this->bhRadius / this->pipe.outRadius;
    this->theta_3 = 1 / (2 * this->theta_1 * this->theta_2);
    this->sigma = (this->grout.k - this->soil.k) / (this->grout.k + this->soil.k);

    this->SubAGG = 15;
    this->AGG = 192;

    // Allocation of all the dynamic arrays
    this->QnMonthlyAgg.dimension(static_cast<int>(this->myRespFactors->maxSimYears * 12), 0.0);
    this->QnHr.dimension(730 + this->AGG + this->SubAGG, 0.0);
    this->QnSubHr.dimension(static_cast<int>((this->SubAGG + 1) * maxTSinHr + 1), 0.0);
    this->LastHourN.dimension(this->SubAGG + 1, 0);

    state.dataGroundHeatExchanger->prevTimeSteps.allocate(static_cast<int>((this->SubAGG + 1) * maxTSinHr + 1));
    state.dataGroundHeatExchanger->prevTimeSteps = 0.0;

    // Initialize ground temperature model and get pointer reference
    this->groundTempModel = GetGroundTempModelAndInit(state,
                                                      UtilityRoutines::MakeUPPERCase(j["undisturbed_ground_temperature_model_type"]),
                                                      UtilityRoutines::MakeUPPERCase(j["undisturbed_ground_temperature_model_name"]));
    if (this->groundTempModel) {
        errorsFound = this->groundTempModel->errorsFound;
    }

    // Check for Errors
    if (errorsFound) {
        ShowFatalError(state, "Errors found in processing input for " + this->moduleName);
    }
}

//******************************************************************************

GLHEVertSingle::GLHEVertSingle(EnergyPlusData &state, std::string const &objName, nlohmann::json const &j)
{
    // Check for duplicates
    for (auto &existingObj : state.dataGroundHeatExchanger->singleBoreholesVector) {
        if (objName == existingObj->name) {
            ShowFatalError(state, "Invalid input for " + this->moduleName + " object: Duplicate name found: " + existingObj->name);
        }
    }

    this->name = objName;
    this->props = GetVertProps(state, UtilityRoutines::MakeUPPERCase(j["ghe_vertical_properties_object_name"]));
    this->xLoc = j["x_location"];
    this->yLoc = j["y_location"];
    this->dl_i = 0.0;
    this->dl_ii = 0.0;
    this->dl_j = 0.0;
}

//******************************************************************************

GLHEVertArray::GLHEVertArray(EnergyPlusData &state, std::string const &objName, nlohmann::json const &j)
{
    // Check for duplicates
    for (auto &existingObj : state.dataGroundHeatExchanger->vertArraysVector) {
        if (objName == existingObj->name) {
            ShowFatalError(state, "Invalid input for " + this->moduleName + " object: Duplicate name found: " + existingObj->name);
        }
    }

    this->name = objName;
    this->props = GetVertProps(state, UtilityRoutines::MakeUPPERCase(j["ghe_vertical_properties_object_name"]));
    this->numBHinXDirection = j["number_of_boreholes_in_x_direction"];
    this->numBHinYDirection = j["number_of_boreholes_in_y_direction"];
    this->bhSpacing = j["borehole_spacing"];
}

//******************************************************************************

GLHEResponseFactors::GLHEResponseFactors(EnergyPlusData &state, std::string const &objName, nlohmann::json const &j)
{

    // Check for duplicates
    for (auto &existingObj : state.dataGroundHeatExchanger->vertPropsVector) {
        if (objName == existingObj->name) {
            ShowFatalError(state, "Invalid input for " + this->moduleName + " object: Duplicate name found: " + existingObj->name);
        }
    }

    this->name = objName;
    this->props = GetVertProps(state, UtilityRoutines::MakeUPPERCase(j["ghe_vertical_properties_object_name"]));
    this->numBoreholes = j["number_of_boreholes"];
    this->gRefRatio = j["g_function_reference_ratio"];
    this->maxSimYears = state.dataEnvrn->MaxNumberSimYears;

    auto const vars = j.at("g_functions");
    std::vector<Real64> tmpLntts;
    std::vector<Real64> tmpGvals;
    for (auto const &var : vars) {
        tmpLntts.push_back(var.at("g_function_ln_t_ts_value"));
        tmpGvals.push_back(var.at("g_function_g_value"));
    }

    bool errorsFound = false;

    if (tmpLntts.size() == tmpLntts.size()) {
        this->numGFuncPairs = static_cast<int>(tmpLntts.size());
    } else {
        errorsFound = true;
        ShowSevereError(state, "Errors found processing response factor input for Response Factor= " + this->name);
        ShowSevereError(state, "Uneven number of g-function pairs");
    }

    this->LNTTS.dimension(this->numGFuncPairs, 0.0);
    this->GFNC.dimension(this->numGFuncPairs, 0.0);

    for (size_t i = 1; i <= tmpLntts.size(); ++i) {
        this->LNTTS(i) = tmpLntts[i - 1];
        this->GFNC(i) = tmpGvals[i - 1];
    }

    if (errorsFound) {
        ShowFatalError(state, "Errors found in processing input for " + this->moduleName);
    }
}

//******************************************************************************

GLHEVertProps::GLHEVertProps(EnergyPlusData &state, std::string const &objName, nlohmann::json const &j)
{

    // Check for duplicates
    for (auto &existingObj : state.dataGroundHeatExchanger->vertPropsVector) {
        if (objName == existingObj->name) {
            ShowFatalError(state, "Invalid input for " + this->moduleName + " object: Duplicate name found: " + existingObj->name);
        }
    }

    // Load data from JSON
    this->name = objName;
    this->bhTopDepth = j["depth_of_top_of_borehole"];
    this->bhLength = j["borehole_length"];
    this->bhDiameter = j["borehole_diameter"];
    this->grout.k = j["grout_thermal_conductivity"];
    this->grout.rhoCp = j["grout_thermal_heat_capacity"];
    this->pipe.k = j["pipe_thermal_conductivity"];
    this->pipe.rhoCp = j["pipe_thermal_heat_capacity"];
    this->pipe.outDia = j["pipe_outer_diameter"];
    this->pipe.thickness = j["pipe_thickness"];
    this->bhUTubeDist = j["u_tube_distance"];

    // Verify u-tube spacing is valid
    if (this->bhUTubeDist < this->pipe.outDia) {
        ShowWarningError(state, "Borehole shank spacing is less than the pipe diameter. U-tube spacing is reference from the u-tube pipe center.");
        ShowWarningError(state, "Shank spacing is set to the outer pipe diameter.");
        this->bhUTubeDist = this->pipe.outDia;
    }

    // Set remaining data derived from previous inputs
    this->pipe.innerDia = this->pipe.outDia - 2 * this->pipe.thickness;
    this->pipe.outRadius = this->pipe.outDia / 2;
    this->pipe.innerRadius = this->pipe.innerDia / 2;
}

//******************************************************************************

std::shared_ptr<GLHEVertProps> GetVertProps(EnergyPlusData &state, std::string const &objectName)
{
    // Check if this instance of this model has already been retrieved
    for (auto &thisProp : state.dataGroundHeatExchanger->vertPropsVector) {
        // Check if the type and name match
        if (objectName == thisProp->name) {
            return thisProp;
        }
    }

    return nullptr;
}

//******************************************************************************

std::shared_ptr<GLHEVertSingle> GetSingleBH(EnergyPlusData &state, std::string const &objectName)
{
    // Check if this instance of this model has already been retrieved
    for (auto &thisBH : state.dataGroundHeatExchanger->singleBoreholesVector) {
        // Check if the type and name match
        if (objectName == thisBH->name) {
            return thisBH;
        }
    }

    return nullptr;
}

//******************************************************************************

std::shared_ptr<GLHEVertArray> GetVertArray(EnergyPlusData &state, std::string const &objectName)
{
    // Check if this instance of this model has already been retrieved
    for (auto &thisProp : state.dataGroundHeatExchanger->vertArraysVector) {
        // Check if the type and name match
        if (objectName == thisProp->name) {
            return thisProp;
        }
    }

    return nullptr;
}

//******************************************************************************

std::shared_ptr<GLHEResponseFactors> GetResponseFactor(EnergyPlusData &state, std::string const &objectName)
{
    // Check if this instance of this model has already been retrieved
    for (auto &thisRF : state.dataGroundHeatExchanger->responseFactorsVector) {
        // Check if the type and name match
        if (objectName == thisRF->name) {
            return thisRF;
        }
    }

    return nullptr;
}

//******************************************************************************

std::shared_ptr<GLHEResponseFactors> BuildAndGetResponseFactorObjectFromArray(EnergyPlusData &state,
                                                                              std::shared_ptr<GLHEVertArray> const &arrayObjectPtr)
{
    // Make new response factor object and store it for later use
    std::shared_ptr<GLHEResponseFactors> thisRF(new GLHEResponseFactors);
    thisRF->name = arrayObjectPtr->name;
    thisRF->props = arrayObjectPtr->props;

    // Build out new instances of the vertical BH objects which correspond to this object
    int xLoc = 0;
    int bhCounter = 0;
    for (int xBH = 1; xBH <= arrayObjectPtr->numBHinXDirection; ++xBH) {
        int yLoc = 0;
        for (int yBH = 1; yBH <= arrayObjectPtr->numBHinYDirection; ++yBH) {
            bhCounter += 1;
            std::shared_ptr<GLHEVertSingle> thisBH(new GLHEVertSingle);
            thisBH->name = format("{} BH {} loc: ({}, {})", thisRF->name, bhCounter, xLoc, yLoc);
            thisBH->props = GetVertProps(state, arrayObjectPtr->props->name);
            thisBH->xLoc = xLoc;
            thisBH->yLoc = yLoc;
            thisRF->myBorholes.push_back(thisBH);
            state.dataGroundHeatExchanger->singleBoreholesVector.push_back(thisBH);
            yLoc += arrayObjectPtr->bhSpacing;
            thisRF->numBoreholes += 1;
        }
        xLoc += arrayObjectPtr->bhSpacing;
    }

    SetupBHPointsForResponseFactorsObject(thisRF);
    state.dataGroundHeatExchanger->responseFactorsVector.push_back(thisRF);
    return thisRF;
}

//******************************************************************************

std::shared_ptr<GLHEResponseFactors>
BuildAndGetResponseFactorsObjectFromSingleBHs(EnergyPlusData &state, std::vector<std::shared_ptr<GLHEVertSingle>> const &singleBHsForRFVect)
{
    // Make new response factor object and store it for later use
    std::shared_ptr<GLHEResponseFactors> thisRF(new GLHEResponseFactors);
    thisRF->name = format("Response Factor Object Auto Generated No: {}", state.dataGroundHeatExchanger->numAutoGeneratedResponseFactors + 1);

    // Make new props object which has the mean values of the other props objects referenced by the individual BH objects
    std::shared_ptr<GLHEVertProps> thisProps(new GLHEVertProps);
    thisProps->name = format("Response Factor Auto Generated Mean Props No: {}", state.dataGroundHeatExchanger->numAutoGeneratedResponseFactors + 1);
    int numBH = singleBHsForRFVect.size();
    for (auto &thisBH : state.dataGroundHeatExchanger->singleBoreholesVector) {
        thisProps->bhDiameter += thisBH->props->bhDiameter;
        thisProps->bhLength += thisBH->props->bhLength;
        thisProps->bhTopDepth += thisBH->props->bhTopDepth;
        thisProps->bhUTubeDist += thisBH->props->bhUTubeDist;

        thisProps->grout.cp += thisBH->props->grout.cp;
        thisProps->grout.diffusivity += thisBH->props->grout.diffusivity;
        thisProps->grout.k += thisBH->props->grout.k;
        thisProps->grout.rho += thisBH->props->grout.rho;
        thisProps->grout.rhoCp += thisBH->props->grout.rhoCp;

        thisProps->pipe.cp += thisBH->props->pipe.cp;
        thisProps->pipe.diffusivity += thisBH->props->pipe.diffusivity;
        thisProps->pipe.k += thisBH->props->pipe.k;
        thisProps->pipe.rho += thisBH->props->pipe.rho;
        thisProps->pipe.rhoCp += thisBH->props->pipe.rhoCp;

        thisProps->pipe.outDia += thisBH->props->pipe.outDia;
        thisProps->pipe.thickness += thisBH->props->pipe.thickness;

        thisProps->pipe.innerDia += (thisBH->props->pipe.outDia - 2 * thisBH->props->pipe.thickness);

        thisRF->myBorholes.push_back(thisBH);
    }

    // normalize by number of bh
    thisProps->bhDiameter /= numBH;
    thisProps->bhLength /= numBH;
    thisProps->bhTopDepth /= numBH;
    thisProps->bhUTubeDist /= numBH;

    thisProps->grout.cp /= numBH;
    thisProps->grout.diffusivity /= numBH;
    thisProps->grout.k /= numBH;
    thisProps->grout.rho /= numBH;
    thisProps->grout.rhoCp /= numBH;

    thisProps->pipe.cp /= numBH;
    thisProps->pipe.diffusivity /= numBH;
    thisProps->pipe.k /= numBH;
    thisProps->pipe.rho /= numBH;
    thisProps->pipe.rhoCp /= numBH;

    thisProps->pipe.outDia /= numBH;
    thisProps->pipe.thickness /= numBH;

    thisProps->pipe.innerDia /= numBH;

    thisRF->props = thisProps;
    thisRF->numBoreholes = thisRF->myBorholes.size();
    state.dataGroundHeatExchanger->vertPropsVector.push_back(thisProps);

    SetupBHPointsForResponseFactorsObject(thisRF);

    state.dataGroundHeatExchanger->responseFactorsVector.push_back(thisRF);

    state.dataGroundHeatExchanger->numAutoGeneratedResponseFactors += 1;

    return thisRF;
}

//******************************************************************************

void SetupBHPointsForResponseFactorsObject(std::shared_ptr<GLHEResponseFactors> &thisRF)
{
    for (auto &thisBH : thisRF->myBorholes) {

        // Using Simpson's rule the number of points (n+1) must be odd, therefore an even number of panels is required
        // Starting from i = 0 to i <= NumPanels produces an odd number of points
        constexpr int numPanels_i = 50;
        constexpr int numPanels_ii = 50;
        constexpr int numPanels_j = 560;

        thisBH->dl_i = thisBH->props->bhLength / numPanels_i;
        for (int i = 0; i <= numPanels_i; ++i) {
            MyCartesian newPoint;
            newPoint.x = thisBH->xLoc;
            newPoint.y = thisBH->yLoc;
            newPoint.z = thisBH->props->bhTopDepth + (i * thisBH->dl_i);
            thisBH->pointLocations_i.push_back(newPoint);
        }

        thisBH->dl_ii = thisBH->props->bhLength / numPanels_ii;
        for (int i = 0; i <= numPanels_ii; ++i) {
            MyCartesian newPoint;
            // For case when bh is being compared to itself, shift points by 1 radius in the horizontal plane
            newPoint.x = thisBH->xLoc + (thisBH->props->bhDiameter / 2.0) / sqrt(2.0);
            newPoint.y = thisBH->yLoc + (thisBH->props->bhDiameter / 2.0) / (-sqrt(2.0));
            newPoint.z = thisBH->props->bhTopDepth + (i * thisBH->dl_ii);
            thisBH->pointLocations_ii.push_back(newPoint);
        }

        thisBH->dl_j = thisBH->props->bhLength / numPanels_j;
        for (int i = 0; i <= numPanels_j; ++i) {
            MyCartesian newPoint;
            newPoint.x = thisBH->xLoc;
            newPoint.y = thisBH->yLoc;
            newPoint.z = thisBH->props->bhTopDepth + (i * thisBH->dl_j);
            thisBH->pointLocations_j.push_back(newPoint);
        }
    }
}

//******************************************************************************

void GLHEBase::onInitLoopEquip(EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation)
{
    this->initGLHESimVars(state);
}

//******************************************************************************

void GLHEBase::simulate(EnergyPlusData &state,
                        [[maybe_unused]] const PlantLocation &calledFromLocation,
                        [[maybe_unused]] bool const FirstHVACIteration,
                        [[maybe_unused]] Real64 &CurLoad,
                        [[maybe_unused]] bool const RunFlag)
{

    if (this->needToSetupOutputVars) {
        this->setupOutput(state);
        this->needToSetupOutputVars = false;
    }

    if (state.dataGlobal->KickOffSimulation) {
        this->initGLHESimVars(state);
    } else {
        this->initGLHESimVars(state);
        this->calcGroundHeatExchanger(state);
        this->updateGHX(state);
    }
}

//******************************************************************************

PlantComponent *GLHEBase::factory(EnergyPlusData &state, int const objectType, std::string const &objectName)
{
    if (state.dataGroundHeatExchanger->GetInput) {
        GetGroundHeatExchangerInput(state);
        state.dataGroundHeatExchanger->GetInput = false;
    }
    if (objectType == DataPlant::TypeOf_GrndHtExchgSystem) {
        for (auto &ghx : state.dataGroundHeatExchanger->verticalGLHE) {
            if (ghx.name == objectName) {
                return &ghx;
            }
        }
    } else if (objectType == DataPlant::TypeOf_GrndHtExchgSlinky) {
        for (auto &ghx : state.dataGroundHeatExchanger->slinkyGLHE) {
            if (ghx.name == objectName) {
                return &ghx;
            }
        }
    }

    // If we didn't find it, fatal
    ShowFatalError(state, "Ground Heat Exchanger Factory: Error getting inputs for GHX named: " + objectName);
    // Shut up the compiler
    return nullptr;
}

//******************************************************************************

std::vector<Real64> GLHEVert::distances(MyCartesian const &point_i, MyCartesian const &point_j)
{
    std::vector<Real64> sumVals;

    // Calculate the distance between points
    sumVals.push_back(pow_2(point_i.x - point_j.x));
    sumVals.push_back(pow_2(point_i.y - point_j.y));
    sumVals.push_back(pow_2(point_i.z - point_j.z));

    Real64 sumTot = 0;
    std::vector<Real64> retVals;
    std::for_each(sumVals.begin(), sumVals.end(), [&](Real64 n) { sumTot += n; });
    retVals.push_back(std::sqrt(sumTot));

    // Calculate distance to mirror point
    sumVals.pop_back();
    sumVals.push_back(pow_2(point_i.z - (-point_j.z)));

    sumTot = 0;
    std::for_each(sumVals.begin(), sumVals.end(), [&](Real64 n) { sumTot += n; });
    retVals.push_back(std::sqrt(sumTot));

    return retVals;
}

//******************************************************************************

Real64 GLHEVert::calcResponse(std::vector<Real64> const &dists, Real64 const &currTime)
{
    Real64 pointToPointResponse = erfc(dists[0] / (2 * sqrt(this->soil.diffusivity * currTime))) / dists[0];
    Real64 pointToReflectedResponse = erfc(dists[1] / (2 * sqrt(this->soil.diffusivity * currTime))) / dists[1];

    return pointToPointResponse - pointToReflectedResponse;
}

//******************************************************************************

Real64 GLHEVert::integral(MyCartesian const &point_i, std::shared_ptr<GLHEVertSingle> const &bh_j, Real64 const &currTime)
{

    // This code could be optimized in a number of ways.
    // The first, most simple way would be to precompute the distances from point i to point j, then store them for reuse.
    // The second, more intensive method would be to break the calcResponse calls out into four different parts.
    // The first point, last point, odd points, and even points. Then multiply the odd/even points by their respective coefficient for the
    // Simpson's method. After that, all points are summed together and divided by 3.

    Real64 sum_f = 0;
    int i = 0;
    int const lastIndex_j = static_cast<int>(bh_j->pointLocations_j.size() - 1u);
    for (auto &point_j : bh_j->pointLocations_j) {
        std::vector<Real64> dists = distances(point_i, point_j);
        Real64 const f = calcResponse(dists, currTime);

        // Integrate using Simpson's
        if (i == 0 || i == lastIndex_j) {
            sum_f += f;
        } else if (isEven(i)) {
            sum_f += 2 * f;
        } else {
            sum_f += 4 * f;
        }

        ++i;
    }

    return (bh_j->dl_j / 3.0) * sum_f;
}

//******************************************************************************

Real64 GLHEVert::doubleIntegral(std::shared_ptr<GLHEVertSingle> const &bh_i, std::shared_ptr<GLHEVertSingle> const &bh_j, Real64 const &currTime)
{

    // Similar optimizations as discussed above could happen here

    if (bh_i == bh_j) {

        Real64 sum_f = 0;
        int i = 0;
        int const lastIndex = static_cast<int>(bh_i->pointLocations_ii.size() - 1u);
        for (auto &thisPoint : bh_i->pointLocations_ii) {

            Real64 f = integral(thisPoint, bh_j, currTime);

            // Integrate using Simpson's
            if (i == 0 || i == lastIndex) {
                sum_f += f;
            } else if (isEven(i)) {
                sum_f += 2 * f;
            } else {
                sum_f += 4 * f;
            }

            ++i;
        }

        return (bh_i->dl_ii / 3.0) * sum_f;

    } else {

        Real64 sum_f = 0;
        int i = 0;
        int const lastIndex = static_cast<int>(bh_i->pointLocations_i.size() - 1u);
        for (auto &thisPoint : bh_i->pointLocations_i) {

            Real64 f = integral(thisPoint, bh_j, currTime);

            // Integrate using Simpson's
            if (i == 0 || i == lastIndex) {
                sum_f += f;
            } else if (isEven(i)) {
                sum_f += 2 * f;
            } else {
                sum_f += 4 * f;
            }

            ++i;
        }

        return (bh_i->dl_i / 3.0) * sum_f;
    }
}

//******************************************************************************

void GLHEVert::calcGFunctions(EnergyPlusData &state)
{

    // No other choice than to calculate the g-functions here
    calcShortTimestepGFunctions(state);
    calcLongTimestepGFunctions(state);
    combineShortAndLongTimestepGFunctions();

    // save data for later
    if (!state.dataSysVars->DisableGLHECaching) {
        myCacheData["Response Factors"]["time"] = std::vector<Real64>(this->myRespFactors->time.begin(), this->myRespFactors->time.end());
        myCacheData["Response Factors"]["LNTTS"] = std::vector<Real64>(this->myRespFactors->LNTTS.begin(), this->myRespFactors->LNTTS.end());
        myCacheData["Response Factors"]["GFNC"] = std::vector<Real64>(this->myRespFactors->GFNC.begin(), this->myRespFactors->GFNC.end());
        writeGLHECacheToFile(state);
    }
}

//******************************************************************************

void GLHEVert::calcLongTimestepGFunctions(EnergyPlusData &state)
{

    constexpr int numDaysInYear(365);
    constexpr Real64 lnttsStepSize = 0.5;

    // Minimum simulation time for which finite line source method is applicable
    constexpr Real64 lntts_min_for_long_timestep = -8.5;

    // Time scale constant
    Real64 const t_s = pow_2(this->bhLength) / (9 * this->soil.diffusivity);

    // Temporary vector for holding the LNTTS vals
    std::vector<Real64> tempLNTTS;

    tempLNTTS.push_back(lntts_min_for_long_timestep);

    // Determine how many g-function pairs to generate based on user defined maximum simulation time
    while (true) {
        Real64 maxPossibleSimTime = exp(tempLNTTS.back()) * t_s;
        if (maxPossibleSimTime <
            this->myRespFactors->maxSimYears * numDaysInYear * DataGlobalConstants::HoursInDay * DataGlobalConstants::SecInHour) {
            tempLNTTS.push_back(tempLNTTS.back() + lnttsStepSize);
        } else {
            break;
        }
    }

    // Setup the arrays
    this->myRespFactors->time.dimension(tempLNTTS.size(), 0.0);
    this->myRespFactors->LNTTS.dimension(tempLNTTS.size(), 0.0);
    this->myRespFactors->GFNC.dimension(tempLNTTS.size(), 0.0);

    int index = 1;
    for (auto &thisLNTTS : tempLNTTS) {
        this->myRespFactors->time(index) = exp(thisLNTTS) * t_s;
        this->myRespFactors->LNTTS(index) = thisLNTTS;
        ++index;
    }

    DisplayString(state, "Initializing GroundHeatExchanger:System: " + this->name);

    // Calculate the g-functions
    for (size_t lntts_index = 1; lntts_index <= this->myRespFactors->LNTTS.size(); ++lntts_index) {
        for (auto &bh_i : this->myRespFactors->myBorholes) {
            Real64 sum_T_ji = 0;
            for (auto &bh_j : this->myRespFactors->myBorholes) {
                sum_T_ji += doubleIntegral(bh_i, bh_j, this->myRespFactors->time(lntts_index));
            }
            this->myRespFactors->GFNC(lntts_index) += sum_T_ji;
        }
        this->myRespFactors->GFNC(lntts_index) /= (2 * this->totalTubeLength);

        std::stringstream ss;
        ss << std::fixed << std::setprecision(1) << float(lntts_index) / this->myRespFactors->LNTTS.size() * 100;

        DisplayString(state, "...progress: " + ss.str() + "%");
    }
}

//******************************************************************************

void GLHEVert::calcShortTimestepGFunctions(EnergyPlusData &state)
{
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    constexpr const char *RoutineName("calcShortTimestepGFunctions");

    enum class CellType
    {
        FLUID,
        CONVECTION,
        PIPE,
        GROUT,
        SOIL
    };

    struct Cell
    {

        ~Cell() = default;

        CellType type;
        Real64 radius_center;
        Real64 radius_outer;
        Real64 radius_inner;
        Real64 thickness;
        Real64 vol;
        Real64 conductivity;
        Real64 rhoCp;
        Real64 temperature;
        Real64 temperature_prev_ts;

        Cell()
            : type(), radius_center(0.0), radius_outer(0.0), radius_inner(0.0), thickness(0.0), vol(0.0), conductivity(0.0), rhoCp(0.0),
              temperature(0.0), temperature_prev_ts(0.0)
        {
        }
    };

    // vector to hold 1-D cells
    std::vector<Cell> Cells;

    // setup pipe, convection, and fluid layer geometries
    constexpr int num_pipe_cells = 4;
    constexpr int num_conv_cells = 1;
    constexpr int num_fluid_cells = 3;
    Real64 const pipe_thickness = this->pipe.thickness;
    Real64 const pcf_cell_thickness = pipe_thickness / num_pipe_cells;
    Real64 const radius_pipe_out = std::sqrt(2) * this->pipe.outRadius;
    Real64 const radius_pipe_in = radius_pipe_out - pipe_thickness;
    Real64 const radius_conv = radius_pipe_in - num_conv_cells * pcf_cell_thickness;
    Real64 const radius_fluid = radius_conv - (num_fluid_cells - 0.5) * pcf_cell_thickness; // accounts for half thickness of boundary cell

    // setup grout layer geometry
    constexpr int num_grout_cells = 27;
    Real64 const radius_grout = this->bhRadius;
    Real64 const grout_cell_thickness = (radius_grout - radius_pipe_out) / num_grout_cells;

    // setup soil layer geometry
    constexpr int num_soil_cells = 500;
    constexpr Real64 radius_soil = 10;
    Real64 const soil_cell_thickness = (radius_soil - radius_grout) / num_soil_cells;

    // use design flow rate
    this->massFlowRate = this->designMassFlow;

    // calculate equivalent thermal resistance between borehole wall and fluid
    Real64 bhResistance = calcBHAverageResistance(state);
    Real64 bhConvectionResistance = calcPipeConvectionResistance(state);
    Real64 bh_equivalent_resistance_tube_grout = bhResistance - bhConvectionResistance / 2.0;
    Real64 bh_equivalent_resistance_convection = bhResistance - bh_equivalent_resistance_tube_grout;

    Real64 initial_temperature = this->inletTemp;
    Real64 cpFluid_init = GetSpecificHeatGlycol(state,
                                                state.dataPlnt->PlantLoop(this->loopNum).FluidName,
                                                initial_temperature,
                                                state.dataPlnt->PlantLoop(this->loopNum).FluidIndex,
                                                RoutineName);
    Real64 fluidDensity_init = GetDensityGlycol(state,
                                                state.dataPlnt->PlantLoop(this->loopNum).FluidName,
                                                initial_temperature,
                                                state.dataPlnt->PlantLoop(this->loopNum).FluidIndex,
                                                RoutineName);

    // initialize the fluid cells
    for (int i = 0; i < num_fluid_cells; ++i) {
        Cell thisCell;
        thisCell.type = CellType::FLUID;
        thisCell.thickness = pcf_cell_thickness;
        thisCell.radius_center = radius_fluid + i * thisCell.thickness;

        // boundary cell is only half thickness
        if (i == 0) {
            thisCell.radius_inner = thisCell.radius_center;
        } else {
            thisCell.radius_inner = thisCell.radius_center - thisCell.thickness / 2.0;
        }

        thisCell.radius_outer = thisCell.radius_center + thisCell.thickness / 2.0;
        thisCell.conductivity = 200;
        thisCell.rhoCp = 2.0 * cpFluid_init * fluidDensity_init * pow_2(this->pipe.innerRadius) / (pow_2(radius_conv) - pow_2(radius_fluid));
        Cells.push_back(thisCell);
    }

    // initialize the convection cells
    for (int i = 0; i < num_conv_cells; ++i) {
        Cell thisCell;
        thisCell.thickness = pcf_cell_thickness;
        thisCell.radius_inner = radius_conv + i * thisCell.thickness;
        thisCell.radius_center = thisCell.radius_inner + thisCell.thickness / 2.0;
        thisCell.radius_outer = thisCell.radius_inner + thisCell.thickness;
        thisCell.conductivity = log(radius_pipe_in / radius_conv) / (2 * DataGlobalConstants::Pi * bh_equivalent_resistance_convection);
        thisCell.rhoCp = 1;
        Cells.push_back(thisCell);
    }

    // initialize pipe cells
    for (int i = 0; i < num_pipe_cells; ++i) {
        Cell thisCell;
        thisCell.type = CellType::PIPE;
        thisCell.thickness = pcf_cell_thickness;
        thisCell.radius_inner = radius_pipe_in + i * thisCell.thickness;
        thisCell.radius_center = thisCell.radius_inner + thisCell.thickness / 2.0;
        thisCell.radius_outer = thisCell.radius_inner + thisCell.thickness;
        thisCell.conductivity = log(radius_grout / radius_pipe_in) / (2 * DataGlobalConstants::Pi * bh_equivalent_resistance_tube_grout);
        thisCell.rhoCp = this->pipe.rhoCp;
        Cells.push_back(thisCell);
    }

    // initialize grout cells
    for (int i = 0; i < num_grout_cells; ++i) {
        Cell thisCell;
        thisCell.type = CellType::GROUT;
        thisCell.thickness = grout_cell_thickness;
        thisCell.radius_inner = radius_pipe_out + i * thisCell.thickness;
        thisCell.radius_center = thisCell.radius_inner + thisCell.thickness / 2.0;
        thisCell.radius_outer = thisCell.radius_inner + thisCell.thickness;
        thisCell.conductivity = log(radius_grout / radius_pipe_in) / (2 * DataGlobalConstants::Pi * bh_equivalent_resistance_tube_grout);
        thisCell.rhoCp = grout.rhoCp;
        Cells.push_back(thisCell);
    }

    // initialize soil cells
    for (int i = 0; i < num_soil_cells; ++i) {
        Cell thisCell;
        thisCell.type = CellType::SOIL;
        thisCell.thickness = soil_cell_thickness;
        thisCell.radius_inner = radius_grout + i * thisCell.thickness;
        thisCell.radius_center = thisCell.radius_inner + thisCell.thickness / 2.0;
        thisCell.radius_outer = thisCell.radius_inner + thisCell.thickness;
        thisCell.conductivity = this->soil.k;
        thisCell.rhoCp = this->soil.rhoCp;
        Cells.push_back(thisCell);
    }

    // other non-geometric specific setup
    for (auto &thisCell : Cells) {
        thisCell.vol = DataGlobalConstants::Pi * (pow_2(thisCell.radius_outer) - pow_2(thisCell.radius_inner));
        thisCell.temperature = initial_temperature;
    }

    // set upper limit of time for the short time-step g-function calcs so there is some overlap
    Real64 const lntts_max_for_short_timestep = -9.0;
    Real64 const t_s = pow_2(this->bhLength) / (9.0 * this->soil.diffusivity);

    Real64 const time_step = 500;
    Real64 const time_max_for_short_timestep = exp(lntts_max_for_short_timestep) * t_s;
    Real64 total_time = 0;

    Real64 const heat_flux = 40.4;

    // time step loop
    while (total_time < time_max_for_short_timestep) {

        for (auto &thisCell : Cells) {
            thisCell.temperature_prev_ts = thisCell.temperature;
        }

        std::vector<Real64> a;
        std::vector<Real64> b;
        std::vector<Real64> c;
        std::vector<Real64> d;

        // setup tdma matrices
        int num_cells = Cells.size();
        for (int cell_index = 0; cell_index < num_cells; ++cell_index) {
            if (cell_index == 0) {
                // heat flux BC

                auto &thisCell = Cells[cell_index];
                auto &eastCell = Cells[cell_index + 1];

                Real64 FE1 = log(thisCell.radius_outer / thisCell.radius_center) / (2 * DataGlobalConstants::Pi * thisCell.conductivity);
                Real64 FE2 = log(eastCell.radius_center / eastCell.radius_inner) / (2 * DataGlobalConstants::Pi * eastCell.conductivity);
                Real64 AE = 1 / (FE1 + FE2);

                Real64 AD = thisCell.rhoCp * thisCell.vol / time_step;

                a.push_back(0);
                b.push_back(-AE / AD - 1);
                c.push_back(AE / AD);
                d.push_back(-thisCell.temperature_prev_ts - heat_flux / AD);

            } else if (cell_index == num_cells - 1) {
                // const ground temp bc

                auto &thisCell = Cells[cell_index];

                a.push_back(0);
                b.push_back(1);
                c.push_back(0);
                d.push_back(thisCell.temperature_prev_ts);

            } else {
                // all other cells

                auto &westCell = Cells[cell_index - 1];
                auto &thisCell = Cells[cell_index];
                auto &eastCell = Cells[cell_index + 1];

                Real64 FE1 = log(thisCell.radius_outer / thisCell.radius_center) / (2 * DataGlobalConstants::Pi * thisCell.conductivity);
                Real64 FE2 = log(eastCell.radius_center / eastCell.radius_inner) / (2 * DataGlobalConstants::Pi * eastCell.conductivity);
                Real64 AE = 1 / (FE1 + FE2);

                Real64 FW1 = log(westCell.radius_outer / westCell.radius_center) / (2 * DataGlobalConstants::Pi * westCell.conductivity);
                Real64 FW2 = log(thisCell.radius_center / thisCell.radius_inner) / (2 * DataGlobalConstants::Pi * thisCell.conductivity);
                Real64 AW = -1 / (FW1 + FW2);

                Real64 AD = thisCell.rhoCp * thisCell.vol / time_step;

                a.push_back(-AW / AD);
                b.push_back(AW / AD - AE / AD - 1);
                c.push_back(AE / AD);
                d.push_back(-thisCell.temperature_prev_ts);
            }
        } // end tdma setup

        // solve for new temperatures
        std::vector<Real64> new_temps = TDMA(a, b, c, d);

        for (int cell_index = 0; cell_index < num_cells; ++cell_index) {
            Cells[cell_index].temperature = new_temps[cell_index];
        }

        // calculate bh wall temp
        Real64 T_bhWall = 0.0;
        for (int cell_index = 0; cell_index < num_cells; ++cell_index) {
            auto &leftCell = Cells[cell_index];
            auto &rightCell = Cells[cell_index + 1];

            if (leftCell.type == CellType::GROUT && rightCell.type == CellType::SOIL) {

                Real64 left_conductance = 2 * DataGlobalConstants::Pi * leftCell.conductivity / log(leftCell.radius_outer / leftCell.radius_inner);
                Real64 right_conductance =
                    2 * DataGlobalConstants::Pi * rightCell.conductivity / log(rightCell.radius_center / leftCell.radius_inner);

                T_bhWall =
                    (left_conductance * leftCell.temperature + right_conductance * rightCell.temperature) / (left_conductance + right_conductance);
                break;
            }
        }

        total_time += time_step;

        GFNC_shortTimestep.push_back(2 * DataGlobalConstants::Pi * this->soil.k *
                                     ((Cells[0].temperature - initial_temperature) / heat_flux - bhResistance));
        LNTTS_shortTimestep.push_back(log(total_time / t_s));

    } // end timestep loop
}

//******************************************************************************

std::vector<Real64> TDMA(std::vector<Real64> a, std::vector<Real64> b, std::vector<Real64> c, std::vector<Real64> d)
{
    // from: https://en.wikibooks.org/wiki/Algorithm_Implementation/Linear_Algebra/Tridiagonal_matrix_algorithm#C.2B.2B

    int n = static_cast<int>(d.size() - 1u);

    c[0] /= b[0];
    d[0] /= b[0];

    for (int i = 1; i < n; ++i) {
        c[i] /= b[i] - a[i] * c[i - 1];
        d[i] = (d[i] - a[i] * d[i - 1]) / (b[i] - a[i] * c[i - 1]);
    }

    d[n] = (d[n] - a[n] * d[n - 1]) / (b[n] - a[n] * c[n - 1]);

    for (int i = n; i-- > 0;) {
        d[i] -= c[i] * d[i + 1];
    }

    return d;
}

//******************************************************************************

void GLHEVert::combineShortAndLongTimestepGFunctions()
{
    std::vector<Real64> GFNC_combined;
    std::vector<Real64> LNTTS_combined;

    Real64 const t_s = pow_2(this->bhLength) / (9.0 * this->soil.diffusivity);

    // Nothing to do. Just put the short time step g-functions on the combined vector
    int num_shortTimestepGFunctions = GFNC_shortTimestep.size();
    for (int index_shortTS = 0; index_shortTS < num_shortTimestepGFunctions; ++index_shortTS) {
        GFNC_combined.push_back(GFNC_shortTimestep[index_shortTS]);
        LNTTS_combined.push_back(LNTTS_shortTimestep[index_shortTS]);
    }

    // Add the rest of the long time-step g-functions to the combined curve
    for (int index_longTS = this->myRespFactors->GFNC.l(); index_longTS <= this->myRespFactors->GFNC.u(); ++index_longTS) {
        GFNC_combined.push_back(this->myRespFactors->GFNC(index_longTS));
        LNTTS_combined.push_back(this->myRespFactors->LNTTS(index_longTS));
    }

    // Move combined values into right data struct
    this->myRespFactors->time.deallocate();
    this->myRespFactors->LNTTS.deallocate();
    this->myRespFactors->GFNC.deallocate();

    this->myRespFactors->time.dimension(GFNC_combined.size(), 0.0);
    this->myRespFactors->LNTTS.dimension(GFNC_combined.size(), 0.0);
    this->myRespFactors->GFNC.dimension(GFNC_combined.size(), 0.0);

    for (unsigned int index = 0; index < GFNC_combined.size(); ++index) {
        this->myRespFactors->time[index] = exp(LNTTS_combined[index]) * t_s;
        this->myRespFactors->LNTTS[index] = LNTTS_combined[index];
        this->myRespFactors->GFNC[index] = GFNC_combined[index];
    }
}

void GLHEBase::makeThisGLHECacheAndCompareWithFileCache(EnergyPlusData &state)
{
    if (!state.dataSysVars->DisableGLHECaching) {
        makeThisGLHECacheStruct();
        readCacheFileAndCompareWithThisGLHECache(state);
    }
}

//******************************************************************************

void GLHEVert::makeThisGLHECacheStruct()
{
    // For convenience
    auto &d = myCacheData["Phys Data"];

    d["Flow Rate"] = this->designFlow;
    d["Soil k"] = this->soil.k;
    d["Soil rhoCp"] = this->soil.rhoCp;
    d["BH Top Depth"] = this->myRespFactors->props->bhTopDepth;
    d["BH Length"] = this->myRespFactors->props->bhLength;
    d["BH Diameter"] = this->myRespFactors->props->bhDiameter;
    d["Grout k"] = this->myRespFactors->props->grout.k;
    d["Grout rhoCp"] = this->myRespFactors->props->grout.rhoCp;
    d["Pipe k"] = this->myRespFactors->props->pipe.k;
    d["Pipe rhoCP"] = this->myRespFactors->props->pipe.rhoCp;
    d["Pipe Diameter"] = this->myRespFactors->props->pipe.outDia;
    d["Pipe Thickness"] = this->myRespFactors->props->pipe.thickness;
    d["U-tube Dist"] = this->myRespFactors->props->bhUTubeDist;
    d["Max Simulation Years"] = this->myRespFactors->maxSimYears;

    int i = 0;
    for (auto &thisBH : this->myRespFactors->myBorholes) {
        ++i;
        auto &d_bh = d["BH Data"][format("BH {}", i)];
        d_bh["X-Location"] = thisBH->xLoc;
        d_bh["Y-Location"] = thisBH->yLoc;
    }
}

//******************************************************************************

void GLHEVert::readCacheFileAndCompareWithThisGLHECache(EnergyPlusData &state)
{
    // For convenience
    using json = nlohmann::json;

    if (!FileSystem::fileExists(state.dataStrGlobals->outputGLHEFilePath)) {
        // if the file doesn't exist, there are no data to read
        return;
    } else {
        // file exists -- read data and load if possible

        // open file
        std::ifstream ifs(state.dataStrGlobals->outputGLHEFilePath);

        // create empty json object
        json json_in;

        // read json_in data
        try {
            ifs >> json_in;
            ifs.close();
        } catch (...) {
            if (!json_in.empty()) {
                // file exists, is not empty, but failed for some other reason
                ShowWarningError(state, state.dataStrGlobals->outputGLHEFilePath.string() + " contains invalid file format");
            }
            ifs.close();
            return;
        }

        for (auto &existing_data : json_in) {
            if (myCacheData["Phys Data"] == existing_data["Phys Data"]) {
                myCacheData["Response Factors"] = existing_data["Response Factors"];
                gFunctionsExist = true;
                break;
            }
        }

        if (gFunctionsExist) {

            // Setup the arrays
            int numEntries = myCacheData["Response Factors"]["LNTTS"].size();

            this->myRespFactors->time.dimension(numEntries, 0.0);
            this->myRespFactors->LNTTS.dimension(numEntries, 0.0);
            this->myRespFactors->GFNC.dimension(numEntries, 0.0);

            // Populate the time array
            int index = 1;
            auto &j_time = myCacheData["Response Factors"]["time"];
            for (auto &it : j_time) {
                this->myRespFactors->time(index) = it;
                ++index;
            }

            // Populate the lntts array
            index = 1;
            auto &j_lntts = myCacheData["Response Factors"]["LNTTS"];
            for (auto &j_lntt : j_lntts) {
                this->myRespFactors->LNTTS(index) = j_lntt;
                ++index;
            }

            // Populate the g-function array
            index = 1;
            auto &j_gfnc = myCacheData["Response Factors"]["GFNC"];
            for (auto &it : j_gfnc) {
                this->myRespFactors->GFNC(index) = it;
                ++index;
            }
        }
    }
}

//******************************************************************************

void GLHEVert::writeGLHECacheToFile(EnergyPlusData &state) const
{

    // For convenience
    using json = nlohmann::json;

    if (FileSystem::fileExists(state.dataStrGlobals->outputGLHEFilePath)) {
        // file exists -- add data

        // open file
        std::ifstream ifs(state.dataStrGlobals->outputGLHEFilePath);

        // create empty json object
        json json_in;

        // read json_in data
        try {
            ifs >> json_in;
            ifs.close();
        } catch (...) {
            if (!json_in.empty()) {
                // file exists, is not empty, but failed for some other reason
                ShowWarningError(state, "Error reading from " + state.dataStrGlobals->outputGLHEFilePath.string());
                ShowWarningError(state, "Data from previous " + state.dataStrGlobals->outputGLHEFilePath.string() + " not saved");
            }
            ifs.close();
        }

        // empty json object for output writing
        json json_out;

        // add existing data to json_out
        int i = 0;
        for (auto &existing_data : json_in) {
            ++i;
            std::string case_name = format("GHLE {}", i);
            json_out[case_name] = existing_data;
        }

        // add current data
        std::string case_name = format("GHLE {}", i + 1);
        json_out[case_name] = myCacheData;

        if (state.files.outputControl.glhe) {
            // open output file
            std::ofstream ofs;
            ofs.open(state.dataStrGlobals->outputGLHEFilePath);
            // write data to file, set spacing at 2
            ofs << std::setw(2) << json_out;
            // don't forget to close
            ofs.close();
        }

    } else {
        // file doesn't exist -- add data

        // empty json object for output writing
        json json_out;

        // add current data
        std::string case_name = "GHLE 1";
        json_out[case_name] = myCacheData;

        if (state.files.outputControl.glhe) {
            // open output file
            std::ofstream ofs;
            ofs.open(state.dataStrGlobals->outputGLHEFilePath);
            // write data to file, set spacing at 2
            ofs << std::setw(2) << json_out;
            // don't forget to close
            ofs.close();
        }
    }
}

//******************************************************************************

void GLHESlinky::calcGFunctions(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // calculates g-functions for the slinky ground heat exchanger model

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 tLg_max(0.0);
    constexpr Real64 tLg_min(-2);
    constexpr Real64 tLg_grid(0.25);
    constexpr Real64 ts(3600);
    Real64 tLg;
    constexpr Real64 convertYearsToSeconds(356 * 24 * 60 * 60);
    Real64 fraction;
    Array2D<Real64> valStored({0, this->numTrenches}, {0, this->numCoils}, -1.0);
    int I0;
    int J0;

    DisplayString(state, "Initializing GroundHeatExchanger:Slinky: " + this->name);

    this->X0.allocate(this->numCoils);
    this->Y0.allocate(this->numTrenches);

    // Calculate the number of g-functions required
    tLg_max = std::log10(this->maxSimYears * convertYearsToSeconds / ts);
    int NPairs = static_cast<int>((tLg_max - tLg_min) / (tLg_grid) + 1);

    // Allocate and setup g-function arrays
    this->myRespFactors->GFNC.allocate(NPairs);
    this->myRespFactors->LNTTS.allocate(NPairs);
    this->QnMonthlyAgg.allocate(static_cast<int>(this->maxSimYears * 12));
    this->QnHr.allocate(730 + this->AGG + this->SubAGG);
    this->QnSubHr.allocate(static_cast<int>((this->SubAGG + 1) * maxTSinHr + 1));
    this->LastHourN.allocate(this->SubAGG + 1);

    for (int i = 1; i <= NPairs; ++i) {
        this->myRespFactors->GFNC(i) = 0.0;
        this->myRespFactors->LNTTS(i) = 0.0;
    }

    // Calculate the number of loops (per trench) and number of trenches to be involved
    // Due to the symmetry of a slinky GHX field, we need only calculate about
    // on quarter of the rings' tube wall temperature perturbation to get the
    // mean wall temperature perturbation of the entire slinky GHX field.
    int numLC = std::ceil(this->numCoils / 2.0);
    int numRC = std::ceil(this->numTrenches / 2.0);

    // Calculate coordinates (X0, Y0, Z0) of a ring's center
    for (int coil = 1; coil <= this->numCoils; ++coil) {
        this->X0(coil) = coilPitch * (coil - 1);
    }
    for (int trench = 1; trench <= this->numTrenches; ++trench) {
        this->Y0(trench) = (trench - 1) * this->trenchSpacing;
    }
    this->Z0 = this->coilDepth;

    // If number of trenches is greater than 1, one quarter of the rings are involved.
    // If number of trenches is 1, one half of the rings are involved.
    if (this->numTrenches > 1) {
        fraction = 0.25;
    } else {
        fraction = 0.5;
    }

    // Calculate the corresponding time of each temperature response factor
    for (int NT = 1; NT <= NPairs; ++NT) {
        tLg = tLg_min + tLg_grid * (NT - 1);
        Real64 t = std::pow(10, tLg) * ts;

        // Set the average temperature response of the whole field to zero
        Real64 gFunc = 0;

        valStored = -1.0;

        for (int m1 = 1; m1 <= numRC; ++m1) {
            for (int n1 = 1; n1 <= numLC; ++n1) {
                for (int m = 1; m <= this->numTrenches; ++m) {
                    for (int n = 1; n <= this->numCoils; ++n) {

                        // Zero out val after each iteration
                        Real64 doubleIntegralVal = 0.0;
                        Real64 midFieldVal = 0.0;

                        // Calculate the distance between ring centers
                        Real64 disRing = distToCenter(m, n, m1, n1);

                        // Save mm1 and nn1
                        int mm1 = std::abs(m - m1);
                        int nn1 = std::abs(n - n1);

                        // If we're calculating a ring's temperature response to itself as a ring source,
                        // then we need some extra effort in calculating the double integral
                        if (m1 == m && n1 == n) {
                            I0 = 33;
                            J0 = 1089;
                        } else {
                            I0 = 33;
                            J0 = 561;
                        }

                        Real64 gFuncin;

                        // if the ring(n1, m1) is the near-field ring of the ring(n,m)
                        if (disRing <= 2.5 + this->coilDiameter) {
                            // if no calculated value has been stored
                            if (valStored(mm1, nn1) < 0) {
                                doubleIntegralVal = doubleIntegral(m, n, m1, n1, t, I0, J0);
                                valStored(mm1, nn1) = doubleIntegralVal;
                                // else: if a stored value is found for the combination of (m, n, m1, n1)
                            } else {
                                doubleIntegralVal = valStored(mm1, nn1);
                            }

                            // due to symmetry, the temperature response of ring(n1, m1) should be 0.25, 0.5, or 1 times its calculated value
                            if (!isEven(this->numTrenches) && !isEven(this->numCoils) && m1 == numRC && n1 == numLC && this->numTrenches > 1.5) {
                                gFuncin = 0.25 * doubleIntegralVal;
                            } else if (!isEven(this->numTrenches) && m1 == numRC && this->numTrenches > 1.5) {
                                gFuncin = 0.5 * doubleIntegralVal;
                            } else if (!isEven(this->numCoils) && n1 == numLC) {
                                gFuncin = 0.5 * doubleIntegralVal;
                            } else {
                                gFuncin = doubleIntegralVal;
                            }

                            // if the ring(n1, m1) is in the far-field or the ring(n,m)
                        } else if (disRing > (10 + this->coilDiameter)) {
                            gFuncin = 0;

                            // else the ring(n1, m1) is in the middle-field of the ring(n,m)
                        } else {
                            // if no calculated value have been stored
                            if (valStored(mm1, nn1) < 0.0) {
                                midFieldVal = midFieldResponseFunction(m, n, m1, n1, t);
                                valStored(mm1, nn1) = midFieldVal;
                                // if a stored value is found for the combination of (m, n, m1, n1), then
                            } else {
                                midFieldVal = valStored(mm1, nn1);
                            }

                            // due to symmetry, the temperature response of ring(n1, m1) should be 0.25, 0.5, or 1 times its calculated value
                            if (!isEven(this->numTrenches) && !isEven(this->numCoils) && m1 == numRC && n1 == numLC && this->numTrenches > 1.5) {
                                gFuncin = 0.25 * midFieldVal;
                            } else if (!isEven(this->numTrenches) && m1 == numRC && this->numTrenches > 1.5) {
                                gFuncin = 0.5 * midFieldVal;
                            } else if (!isEven(this->numCoils) && n1 == numLC) {
                                gFuncin = 0.5 * midFieldVal;
                            } else {
                                gFuncin = midFieldVal;
                            }
                        }

                        gFunc += gFuncin;

                    } // n
                }     // m
            }         // n1
        }             // m1

        this->myRespFactors->GFNC(NT) =
            (gFunc * (this->coilDiameter / 2.0)) / (4 * DataGlobalConstants::Pi * fraction * this->numTrenches * this->numCoils);
        this->myRespFactors->LNTTS(NT) = tLg;

    } // NT time
}

//******************************************************************************

void GLHESlinky::makeThisGLHECacheStruct()
{
}

//******************************************************************************

void GLHESlinky::readCacheFileAndCompareWithThisGLHECache([[maybe_unused]] EnergyPlusData &state)
{
}

//******************************************************************************

Real64
GLHESlinky::nearFieldResponseFunction(int const m, int const n, int const m1, int const n1, Real64 const eta, Real64 const theta, Real64 const t)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the temperature response of from one near-field point to another

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 distance1 = distance(m, n, m1, n1, eta, theta);
    Real64 sqrtAlphaT = std::sqrt(this->soil.diffusivity * t);

    if (!verticalConfig) {

        Real64 sqrtDistDepth = std::sqrt(pow_2(distance1) + 4 * pow_2(this->coilDepth));
        Real64 errFunc1 = std::erfc(0.5 * distance1 / sqrtAlphaT);
        Real64 errFunc2 = std::erfc(0.5 * sqrtDistDepth / sqrtAlphaT);

        return errFunc1 / distance1 - errFunc2 / sqrtDistDepth;

    } else {

        Real64 distance2 = distanceToFictRing(m, n, m1, n1, eta, theta);
        Real64 errFunc1 = std::erfc(0.5 * distance1 / sqrtAlphaT);
        Real64 errFunc2 = std::erfc(0.5 * distance2 / sqrtAlphaT);

        return errFunc1 / distance1 - errFunc2 / distance2;
    }
}

//******************************************************************************

Real64 GLHESlinky::midFieldResponseFunction(int const m, int const n, int const m1, int const n1, Real64 const t)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the temperature response of from one mid-field point to another

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 sqrtAlphaT = std::sqrt(this->soil.diffusivity * t);

    Real64 distance = distToCenter(m, n, m1, n1);
    Real64 sqrtDistDepth = std::sqrt(pow_2(distance) + 4 * pow_2(this->coilDepth));

    Real64 errFunc1 = std::erfc(0.5 * distance / sqrtAlphaT);
    Real64 errFunc2 = std::erfc(0.5 * sqrtDistDepth / sqrtAlphaT);

    return 4 * pow_2(DataGlobalConstants::Pi) * (errFunc1 / distance - errFunc2 / sqrtDistDepth);
}

//******************************************************************************

Real64 GLHESlinky::distance(int const m, int const n, int const m1, int const n1, Real64 const eta, Real64 const theta)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the distance between any two points on any two loops

    Real64 const cos_theta = std::cos(theta);
    Real64 const sin_theta = std::sin(theta);
    Real64 const cos_eta = std::cos(eta);
    Real64 const sin_eta = std::sin(eta);

    Real64 x = this->X0(n) + cos_theta * (this->coilDiameter / 2.0);
    Real64 y = this->Y0(m) + sin_theta * (this->coilDiameter / 2.0);

    Real64 xIn = this->X0(n1) + cos_eta * (this->coilDiameter / 2.0 - this->pipe.outRadius);
    Real64 yIn = this->Y0(m1) + sin_eta * (this->coilDiameter / 2.0 - this->pipe.outRadius);

    Real64 xOut = this->X0(n1) + cos_eta * (this->coilDiameter / 2.0 + this->pipe.outRadius);
    Real64 yOut = this->Y0(m1) + sin_eta * (this->coilDiameter / 2.0 + this->pipe.outRadius);

    if (!verticalConfig) {

        return 0.5 * std::sqrt(pow_2(x - xIn) + pow_2(y - yIn)) + 0.5 * std::sqrt(pow_2(x - xOut) + pow_2(y - yOut));

    } else {

        Real64 z = this->Z0 + sin_theta * (this->coilDiameter / 2.0);

        Real64 zIn = this->Z0 + sin_eta * (this->coilDiameter / 2.0 - this->pipe.outRadius);
        Real64 zOut = this->Z0 + sin_eta * (this->coilDiameter / 2.0 + this->pipe.outRadius);

        return 0.5 * std::sqrt(pow_2(x - xIn) + pow_2(this->Y0(m1) - this->Y0(m)) + pow_2(z - zIn)) +
               0.5 * std::sqrt(pow_2(x - xOut) + pow_2(this->Y0(m1) - this->Y0(m)) + pow_2(z - zOut));
    }
}

//******************************************************************************

Real64 GLHESlinky::distanceToFictRing(int const m, int const n, int const m1, int const n1, Real64 const eta, Real64 const theta)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the distance between any two points between real and fictitious rings

    Real64 const sin_theta = std::sin(theta);
    Real64 const cos_theta = std::cos(theta);
    Real64 const sin_eta = std::sin(eta);
    Real64 const cos_eta = std::cos(eta);

    Real64 x = this->X0(n) + cos_theta * (this->coilDiameter / 2.0);
    // Real64 y = Y0( m ) + sin_theta * (coilDiameter / 2.0);
    Real64 z = this->Z0 + sin_theta * (this->coilDiameter / 2.0) + 2 * this->coilDepth;

    Real64 xIn = this->X0(n1) + cos_eta * (this->coilDiameter / 2.0 - this->pipe.outRadius);
    // Real64 yIn = Y0( m1 ) + sin_eta * (coilDiameter / 2.0 - pipe.outRadius);
    Real64 zIn = this->Z0 + sin_eta * (this->coilDiameter / 2.0 - this->pipe.outRadius);

    Real64 xOut = this->X0(n1) + cos_eta * (this->coilDiameter / 2.0 + this->pipe.outRadius);
    // Real64 yOut = Y0( m1 ) + sin_eta * (coilDiameter / 2.0 + outRadius);
    Real64 zOut = this->Z0 + sin_eta * (this->coilDiameter / 2.0 + this->pipe.outRadius);

    return 0.5 * std::sqrt(pow_2(x - xIn) + pow_2(this->Y0(m1) - this->Y0(m)) + pow_2(z - zIn)) +
           0.5 * std::sqrt(pow_2(x - xOut) + pow_2(this->Y0(m1) - this->Y0(m)) + pow_2(z - zOut));
}

//******************************************************************************

Real64 GLHESlinky::distToCenter(int const m, int const n, int const m1, int const n1)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the center-to-center distance between rings

    return std::sqrt(pow_2(this->X0(n) - this->X0(n1)) + pow_2(this->Y0(m) - this->Y0(m1)));
}

//******************************************************************************

inline bool GLHEBase::isEven(int const val)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Determines if an integer is even

    if (val % 2 == 0) {
        return true;
    } else {
        return false;
    }
}

//******************************************************************************

Real64 GLHESlinky::integral(int const m, int const n, int const m1, int const n1, Real64 const t, Real64 const eta, Real64 const J0)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Integrates the temperature response at one point based on
    // input from other points

    // METHODOLOGY EMPLOYED:
    // Simpson's 1/3 rule of integration

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 sumIntF(0.0);
    Real64 theta(0.0);
    constexpr Real64 theta1(0.0);
    constexpr Real64 theta2(2 * DataGlobalConstants::Pi);
    Array1D<Real64> f(J0, 0.0);

    Real64 h = (theta2 - theta1) / (J0 - 1);

    // Calculate the function at various equally spaced x values
    for (int j = 1; j <= J0; ++j) {

        theta = theta1 + (j - 1) * h;

        f(j) = nearFieldResponseFunction(m, n, m1, n1, eta, theta, t);

        if (j == 1 || j == J0) {
            f(j) = f(j);
        } else if (isEven(j)) {
            f(j) = 4 * f(j);
        } else {
            f(j) = 2 * f(j);
        }

        sumIntF += f(j);
    }

    return (h / 3) * sumIntF;
}

//******************************************************************************

Real64 GLHESlinky::doubleIntegral(int const m, int const n, int const m1, int const n1, Real64 const t, int const I0, int const J0)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Integrates the temperature response at one point based on
    // input from other points

    // METHODOLOGY EMPLOYED:
    // Simpson's 1/3 rule of integration

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    constexpr Real64 eta1(0.0);
    constexpr Real64 eta2(2 * DataGlobalConstants::Pi);

    Real64 sumIntF(0.0);
    Array1D<Real64> g(I0, 0.0);

    Real64 h = (eta2 - eta1) / (I0 - 1);

    // Calculates the value of the function at various equally spaced values
    for (int i = 1; i <= I0; ++i) {

        Real64 eta = eta1 + (i - 1) * h;
        g(i) = integral(m, n, m1, n1, t, eta, J0);

        if (i == 1 || i == I0) {
            g(i) = g(i);
        } else if (isEven(i)) {
            g(i) = 4 * g(i);
        } else {
            g(i) = 2 * g(i);
        }

        sumIntF += g(i);
    }

    return (h / 3) * sumIntF;
}

//******************************************************************************

void GLHEVert::getAnnualTimeConstant()
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate annual time constant for ground conduction

    constexpr Real64 hrInYear(8760);

    this->timeSS = (pow_2(this->bhLength) / (9.0 * this->soil.diffusivity)) / DataGlobalConstants::SecInHour / hrInYear;
    this->timeSSFactor = this->timeSS * 8760.0;
}

//******************************************************************************

void GLHESlinky::getAnnualTimeConstant()
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate annual time constant for ground conduction

    this->timeSSFactor = 1.0;
}

//******************************************************************************

void GLHEBase::calcGroundHeatExchanger(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Dan Fisher
    //       DATE WRITTEN:    August, 2000
    //       MODIFIED         Arun Murugappan
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // This is the main routine to simulate the operation of vertical
    // closed-loop ground heat exchangers (GLHE).

    // METHODOLOGY EMPLOYED:
    // The borehole and fluid temperatures are calculated from the response to
    // the current heat transfer rate and the response to the history of past
    // applied heat pulses. The response to each pulse is calculated from a non-
    // dimensionalized response function, or G-function, that is specific to the
    // given borehole field arrangement, depth and spacing. The data defining
    // this function is read from input.
    // The heat pulse histories need to be recorded over an extended period (months).
    // To aid computational efficiency past pulses are continuously aggregated into
    // equivalent heat pulses of longer duration, as each pulse becomes less recent.

    // REFERENCES:
    // Eskilson, P. 'Thermal Analysis of Heat Extraction Boreholes' Ph.D. Thesis:
    //   Dept. of Mathematical Physics, University of Lund, Sweden, June 1987.
    // Yavuzturk, C., J.D. Spitler. 1999. 'A Short Time Step Response Factor Model
    //   for Vertical Ground Loop Heat Exchangers.' ASHRAE Transactions. 105(2): 475-485.

    // Using/Aliasing
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE ARGUMENT DEFINITIONS
    constexpr const char *RoutineName("CalcGroundHeatExchanger");

    // LOCAL PARAMETERS
    Real64 fluidAveTemp;
    Real64 tmpQnSubHourly; // current Qn sub-hourly value
    Real64 sumTotal(0.0);  // sum of all the Qn (load) blocks

    // Calculate G-Functions
    if (this->firstTime) {
        if (!gFunctionsExist) {
            makeThisGLHECacheAndCompareWithFileCache(state);
            if (!gFunctionsExist) {
                calcGFunctions(state);
                gFunctionsExist = true;
            }
        }
        this->firstTime = false;
    }

    this->inletTemp = state.dataLoopNodes->Node(this->inletNodeNum).Temp;

    Real64 cpFluid = GetSpecificHeatGlycol(
        state, state.dataPlnt->PlantLoop(this->loopNum).FluidName, this->inletTemp, state.dataPlnt->PlantLoop(this->loopNum).FluidIndex, RoutineName);

    Real64 kGroundFactor = 2.0 * DataGlobalConstants::Pi * this->soil.k;

    // Get time constants
    getAnnualTimeConstant();

    if (triggerDesignDayReset && state.dataGlobal->WarmupFlag) updateCurSimTime = true;
    if (state.dataGlobal->DayOfSim == 1 && updateCurSimTime) {
        state.dataGroundHeatExchanger->currentSimTime = 0.0;
        state.dataGroundHeatExchanger->prevTimeSteps = 0.0;
        this->QnHr = 0.0;
        this->QnMonthlyAgg = 0.0;
        this->QnSubHr = 0.0;
        this->LastHourN = 1;
        state.dataGroundHeatExchanger->N = 1;
        updateCurSimTime = false;
        triggerDesignDayReset = false;
    }

    state.dataGroundHeatExchanger->currentSimTime = (state.dataGlobal->DayOfSim - 1) * 24 + state.dataGlobal->HourOfDay - 1 +
                                                    (state.dataGlobal->TimeStep - 1) * state.dataGlobal->TimeStepZone +
                                                    state.dataHVACGlobal->SysTimeElapsed; //+ TimeStepsys
    state.dataGroundHeatExchanger->locHourOfDay =
        static_cast<int>(mod(state.dataGroundHeatExchanger->currentSimTime, DataGlobalConstants::HoursInDay) + 1);
    state.dataGroundHeatExchanger->locDayOfSim = static_cast<int>(state.dataGroundHeatExchanger->currentSimTime / 24 + 1);

    if (state.dataGlobal->DayOfSim > 1) {
        updateCurSimTime = true;
    }

    if (!state.dataGlobal->WarmupFlag) {
        triggerDesignDayReset = true;
    }

    if (state.dataGroundHeatExchanger->currentSimTime <= 0.0) {
        state.dataGroundHeatExchanger->prevTimeSteps = 0.0; // This resets history when rounding 24:00 hours during warmup avoids hard crash later
        calcAggregateLoad(state);                           // Just allocates and initializes prevHour array
        return;
    }

    // Store currentSimTime in prevTimeSteps only if a time step occurs
    if (state.dataGroundHeatExchanger->prevTimeSteps(1) != state.dataGroundHeatExchanger->currentSimTime) {
        state.dataGroundHeatExchanger->prevTimeSteps =
            eoshift(state.dataGroundHeatExchanger->prevTimeSteps, -1, state.dataGroundHeatExchanger->currentSimTime);
        ++state.dataGroundHeatExchanger->N;
    }

    if (state.dataGroundHeatExchanger->N != PrevN) {
        PrevN = state.dataGroundHeatExchanger->N;
        this->QnSubHr = eoshift(this->QnSubHr, -1, this->lastQnSubHr);
    }

    calcAggregateLoad(state);

    // Update the heat exchanger resistance each time
    this->HXResistance = calcHXResistance(state);

    if (state.dataGroundHeatExchanger->N == 1) {
        if (this->massFlowRate <= 0.0) {
            tmpQnSubHourly = 0.0;
            fluidAveTemp = this->tempGround;
            this->ToutNew = this->inletTemp;
        } else {
            Real64 gFuncVal = getGFunc(state.dataGroundHeatExchanger->currentSimTime / (this->timeSSFactor));

            Real64 C_1 = (this->totalTubeLength) / (2.0 * this->massFlowRate * cpFluid);
            tmpQnSubHourly = (this->tempGround - this->inletTemp) / (gFuncVal / (kGroundFactor) + this->HXResistance + C_1);
            fluidAveTemp = this->tempGround - tmpQnSubHourly * this->HXResistance;
            this->ToutNew = this->tempGround - tmpQnSubHourly * (gFuncVal / (kGroundFactor) + this->HXResistance - C_1);
        }
    } else {
        // no monthly super position
        if (state.dataGroundHeatExchanger->currentSimTime < (hrsPerMonth + this->AGG + this->SubAGG)) {

            // Calculate the Sub Hourly Superposition

            // same as above for sub-hourly( with no aggregation]
            Real64 sumQnSubHourly = 0.0;
            int IndexN;
            if (int(state.dataGroundHeatExchanger->currentSimTime) < this->SubAGG) {
                IndexN = int(state.dataGroundHeatExchanger->currentSimTime) + 1;
            } else {
                IndexN = this->SubAGG + 1;
            }

            int subHourlyLimit = state.dataGroundHeatExchanger->N - this->LastHourN(IndexN); // Check this when running simulation
            for (int I = 1; I <= subHourlyLimit; ++I) {
                if (I == subHourlyLimit) {
                    if (int(state.dataGroundHeatExchanger->currentSimTime) >= this->SubAGG) {
                        Real64 gFuncVal =
                            getGFunc((state.dataGroundHeatExchanger->currentSimTime - state.dataGroundHeatExchanger->prevTimeSteps(I + 1)) /
                                     (this->timeSSFactor));
                        Real64 RQSubHr = gFuncVal / (kGroundFactor);
                        sumQnSubHourly += (this->QnSubHr(I) - this->QnHr(IndexN)) * RQSubHr;
                    } else {
                        Real64 gFuncVal =
                            getGFunc((state.dataGroundHeatExchanger->currentSimTime - state.dataGroundHeatExchanger->prevTimeSteps(I + 1)) /
                                     (this->timeSSFactor));
                        Real64 RQSubHr = gFuncVal / (kGroundFactor);
                        sumQnSubHourly += this->QnSubHr(I) * RQSubHr;
                    }
                    break;
                }
                // prevTimeSteps(I+1) This is "I+1" because prevTimeSteps(1) = CurrentTimestep
                Real64 gFuncVal = getGFunc((state.dataGroundHeatExchanger->currentSimTime - state.dataGroundHeatExchanger->prevTimeSteps(I + 1)) /
                                           (this->timeSSFactor));
                Real64 RQSubHr = gFuncVal / (kGroundFactor);
                sumQnSubHourly += (this->QnSubHr(I) - this->QnSubHr(I + 1)) * RQSubHr;
            }

            // Calculate the Hourly Superposition
            // same as above for hourly
            Real64 sumQnHourly = 0.0;

            int hourlyLimit = int(state.dataGroundHeatExchanger->currentSimTime);
            for (int I = this->SubAGG + 1; I <= hourlyLimit; ++I) {
                if (I == hourlyLimit) {
                    Real64 gFuncVal = getGFunc(state.dataGroundHeatExchanger->currentSimTime / (this->timeSSFactor));
                    Real64 RQHour = gFuncVal / (kGroundFactor);
                    sumQnHourly += this->QnHr(I) * RQHour;
                    break;
                }
                Real64 gFuncVal = getGFunc((state.dataGroundHeatExchanger->currentSimTime - int(state.dataGroundHeatExchanger->currentSimTime) + I) /
                                           (this->timeSSFactor));
                Real64 RQHour = gFuncVal / (kGroundFactor);
                sumQnHourly += (this->QnHr(I) - this->QnHr(I + 1)) * RQHour;
            }

            // Find the total Sum of the Temperature difference due to all load blocks
            sumTotal = sumQnSubHourly + sumQnHourly;

            // Calculate the sub-hourly temperature due the Last Time steps Load
            Real64 gFuncVal =
                getGFunc((state.dataGroundHeatExchanger->currentSimTime - state.dataGroundHeatExchanger->prevTimeSteps(2)) / (this->timeSSFactor));
            Real64 RQSubHr = gFuncVal / kGroundFactor;

            if (this->massFlowRate <= 0.0) {
                tmpQnSubHourly = 0.0;
                fluidAveTemp = this->tempGround - sumTotal; // Q(N)*RB = 0
                this->ToutNew = this->inletTemp;
            } else {
                // Dr.Sitler's Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
                Real64 C0 = RQSubHr;
                Real64 C1 = this->tempGround - (sumTotal - this->QnSubHr(1) * RQSubHr);
                Real64 C2 = this->totalTubeLength / (2.0 * this->massFlowRate * cpFluid);
                Real64 C3 = this->massFlowRate * cpFluid / (this->totalTubeLength);
                tmpQnSubHourly = (C1 - this->inletTemp) / (this->HXResistance + C0 - C2 + (1 / C3));
                fluidAveTemp = C1 - (C0 + this->HXResistance) * tmpQnSubHourly;
                this->ToutNew = C1 + (C2 - C0 - this->HXResistance) * tmpQnSubHourly;
            }

        } else { // Monthly Aggregation and super position

            // the number of months of simulation elapsed
            int numOfMonths = static_cast<int>((state.dataGroundHeatExchanger->currentSimTime + 1) / hrsPerMonth);

            // The Month up to which the monthly blocks are superposed
            int currentMonth;

            if (state.dataGroundHeatExchanger->currentSimTime < ((numOfMonths)*hrsPerMonth) + this->AGG + this->SubAGG) {
                currentMonth = numOfMonths - 1;
            } else {
                currentMonth = numOfMonths;
            }

            // Monthly superposition
            // tmp variable which holds the sum of the Temperature difference due to Aggregated heat extraction/rejection step
            Real64 sumQnMonthly = 0.0;

            for (int I = 1; I <= currentMonth; ++I) {
                if (I == 1) {
                    Real64 gFuncVal = getGFunc(state.dataGroundHeatExchanger->currentSimTime / (this->timeSSFactor));
                    Real64 RQMonth = gFuncVal / (kGroundFactor);
                    sumQnMonthly += this->QnMonthlyAgg(I) * RQMonth;
                    continue;
                }
                Real64 gFuncVal = getGFunc((state.dataGroundHeatExchanger->currentSimTime - (I - 1) * hrsPerMonth) / (this->timeSSFactor));
                Real64 RQMonth = gFuncVal / (kGroundFactor);
                sumQnMonthly += (this->QnMonthlyAgg(I) - this->QnMonthlyAgg(I - 1)) * RQMonth;
            }

            // Hourly Superposition
            Real64 sumQnHourly = 0.0;
            int hourlyLimit = int(state.dataGroundHeatExchanger->currentSimTime - currentMonth * hrsPerMonth);
            for (int I = 1 + this->SubAGG; I <= hourlyLimit; ++I) {
                if (I == hourlyLimit) {
                    Real64 gFuncVal =
                        getGFunc((state.dataGroundHeatExchanger->currentSimTime - int(state.dataGroundHeatExchanger->currentSimTime) + I) /
                                 (this->timeSSFactor));
                    Real64 RQHour = gFuncVal / (kGroundFactor);
                    sumQnHourly += (this->QnHr(I) - this->QnMonthlyAgg(currentMonth)) * RQHour;
                    break;
                }
                Real64 gFuncVal = getGFunc((state.dataGroundHeatExchanger->currentSimTime - int(state.dataGroundHeatExchanger->currentSimTime) + I) /
                                           (this->timeSSFactor));
                Real64 RQHour = gFuncVal / (kGroundFactor);
                sumQnHourly += (this->QnHr(I) - this->QnHr(I + 1)) * RQHour;
            }

            // sub-hourly Superposition
            int subHourlyLimit = state.dataGroundHeatExchanger->N - this->LastHourN(this->SubAGG + 1);
            Real64 sumQnSubHourly = 0.0;
            for (int I = 1; I <= subHourlyLimit; ++I) {
                if (I == subHourlyLimit) {
                    Real64 gFuncVal = getGFunc((state.dataGroundHeatExchanger->currentSimTime - state.dataGroundHeatExchanger->prevTimeSteps(I + 1)) /
                                               (this->timeSSFactor));
                    Real64 RQSubHr = gFuncVal / (kGroundFactor);
                    sumQnSubHourly += (this->QnSubHr(I) - this->QnHr(this->SubAGG + 1)) * RQSubHr;
                    break;
                }
                Real64 gFuncVal = getGFunc((state.dataGroundHeatExchanger->currentSimTime - state.dataGroundHeatExchanger->prevTimeSteps(I + 1)) /
                                           (this->timeSSFactor));
                Real64 RQSubHr = gFuncVal / (kGroundFactor);
                sumQnSubHourly += (this->QnSubHr(I) - this->QnSubHr(I + 1)) * RQSubHr;
            }

            sumTotal = sumQnMonthly + sumQnHourly + sumQnSubHourly;

            // Calculate the sub-hourly temperature due the Last Time steps Load
            Real64 gFuncVal =
                getGFunc((state.dataGroundHeatExchanger->currentSimTime - state.dataGroundHeatExchanger->prevTimeSteps(2)) / (this->timeSSFactor));
            Real64 RQSubHr = gFuncVal / (kGroundFactor);

            if (this->massFlowRate <= 0.0) {
                tmpQnSubHourly = 0.0;
                fluidAveTemp = this->tempGround - sumTotal; // Q(N)*RB = 0
                this->ToutNew = this->inletTemp;
            } else {
                // Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
                Real64 C0 = RQSubHr;
                Real64 C1 = this->tempGround - (sumTotal - this->QnSubHr(1) * RQSubHr);
                Real64 C2 = this->totalTubeLength / (2 * this->massFlowRate * cpFluid);
                Real64 C3 = this->massFlowRate * cpFluid / (this->totalTubeLength);
                tmpQnSubHourly = (C1 - this->inletTemp) / (this->HXResistance + C0 - C2 + (1 / C3));
                fluidAveTemp = C1 - (C0 + this->HXResistance) * tmpQnSubHourly;
                this->ToutNew = C1 + (C2 - C0 - this->HXResistance) * tmpQnSubHourly;
            }
        } //  end of AGG OR NO AGG
    }     // end of N  = 1 branch
    this->bhTemp = this->tempGround - sumTotal;

    // Load the QnSubHourly Array with a new value at end of every timestep
    this->lastQnSubHr = tmpQnSubHourly;
    this->outletTemp = this->ToutNew;
    this->QGLHE = tmpQnSubHourly * this->totalTubeLength;
    this->aveFluidTemp = fluidAveTemp;
}

//******************************************************************************

void GLHEBase::updateGHX(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED:        na
    //       RE-ENGINEERED:   na

    // PURPOSE OF THIS SUBROUTINE:
    // Updates the outlet node and check for out of bounds temperatures

    // Using/Aliasing
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;
    using PlantUtilities::SafeCopyPlantNode;

    // SUBROUTINE ARGUMENT DEFINITIONS
    constexpr const char *RoutineName("UpdateGroundHeatExchanger");

    constexpr Real64 deltaTempLimit(100.0); // temp limit for warnings

    SafeCopyPlantNode(state, this->inletNodeNum, this->outletNodeNum);

    state.dataLoopNodes->Node(this->outletNodeNum).Temp = this->outletTemp;
    state.dataLoopNodes->Node(this->outletNodeNum).Enthalpy =
        this->outletTemp * GetSpecificHeatGlycol(state,
                                                 state.dataPlnt->PlantLoop(this->loopNum).FluidName,
                                                 this->outletTemp,
                                                 state.dataPlnt->PlantLoop(this->loopNum).FluidIndex,
                                                 RoutineName);

    Real64 GLHEdeltaTemp = std::abs(this->outletTemp - this->inletTemp);

    if (GLHEdeltaTemp > deltaTempLimit && this->numErrorCalls < state.dataGroundHeatExchanger->numVerticalGLHEs && !state.dataGlobal->WarmupFlag) {
        Real64 fluidDensity = GetDensityGlycol(state,
                                               state.dataPlnt->PlantLoop(this->loopNum).FluidName,
                                               this->inletTemp,
                                               state.dataPlnt->PlantLoop(this->loopNum).FluidIndex,
                                               RoutineName);
        this->designMassFlow = this->designFlow * fluidDensity;
        ShowWarningError(state, "Check GLHE design inputs & g-functions for consistency");
        ShowContinueError(state, "For GroundHeatExchanger: " + this->name + "GLHE delta Temp > 100C.");
        ShowContinueError(state, "This can be encountered in cases where the GLHE mass flow rate is either significantly");
        ShowContinueError(state, " lower than the design value, or cases where the mass flow rate rapidly changes.");
        ShowContinueError(state, format("GLHE Current Flow Rate={:.3T}; GLHE Design Flow Rate={:.3T}", this->massFlowRate, this->designMassFlow));
        ++this->numErrorCalls;
    }
}

//******************************************************************************

void GLHEBase::calcAggregateLoad(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Arun Murugappan
    //       DATE WRITTEN:    August, 2000
    //       MODIFIED:        na
    //       RE-ENGINEERED:   na

    // PURPOSE OF THIS SUBROUTINE:
    // Manages the heat transfer history.

    // METHODOLOGY EMPLOYED:
    // The heat pulse histories need to be recorded over an extended period (months).
    // To aid computational efficiency past pulses are continuously aggregated into
    // equivalent heat pulses of longer duration, as each pulse becomes less recent.
    // Past sub-hourly loads are re-aggregated into equivalent hourly and monthly loads.

    // REFERENCES:
    // Eskilson, P. 'Thermal Analysis of Heat Extraction Boreholes' Ph.D. Thesis:
    //   Dept. of Mathematical Physics, University of Lund, Sweden, June 1987.
    // Yavuzturk, C., J.D. Spitler. 1999. 'A Short Time Step Response Factor Model
    //   for Vertical Ground Loop Heat Exchangers. ASHRAE Transactions. 105(2): 475-485.

    if (state.dataGroundHeatExchanger->currentSimTime <= 0.0) return;

    // FOR EVERY HOUR UPDATE THE HOURLY QN this->QnHr(J)
    // THIS IS DONE BY AGGREGATING THE sub-hourly QN FROM THE PREVIOUS HOUR TO UNTIL THE CURRENT HOUR
    // AND STORING IT IN  verticalGLHE(GLHENum)%QnHr(J)

    // sub-hourly Qn IS NOT AGGREGATED . IT IS THE BASIC LOAD
    if (this->prevHour != state.dataGroundHeatExchanger->locHourOfDay) {
        Real64 SumQnHr = 0.0;
        int J;
        for (J = 1; J <= (state.dataGroundHeatExchanger->N - this->LastHourN(1)); ++J) {
            SumQnHr +=
                this->QnSubHr(J) * std::abs(state.dataGroundHeatExchanger->prevTimeSteps(J) - state.dataGroundHeatExchanger->prevTimeSteps(J + 1));
        }
        if (state.dataGroundHeatExchanger->prevTimeSteps(1) != state.dataGroundHeatExchanger->prevTimeSteps(J)) {
            SumQnHr /= std::abs(state.dataGroundHeatExchanger->prevTimeSteps(1) - state.dataGroundHeatExchanger->prevTimeSteps(J));
        } else {
            SumQnHr /= 0.05; // estimated small timestep
        }
        this->QnHr = eoshift(this->QnHr, -1, SumQnHr);
        this->LastHourN = eoshift(this->LastHourN, -1, state.dataGroundHeatExchanger->N);
    }

    // CHECK IF A MONTH PASSES...
    if (mod(((state.dataGroundHeatExchanger->locDayOfSim - 1) * DataGlobalConstants::HoursInDay + (state.dataGroundHeatExchanger->locHourOfDay)),
            hrsPerMonth) == 0 &&
        this->prevHour != state.dataGroundHeatExchanger->locHourOfDay) {
        Real64 MonthNum = static_cast<int>(
            (state.dataGroundHeatExchanger->locDayOfSim * DataGlobalConstants::HoursInDay + state.dataGroundHeatExchanger->locHourOfDay) /
            hrsPerMonth);
        Real64 SumQnMonth = 0.0;
        for (int J = 1; J <= int(hrsPerMonth); ++J) {
            SumQnMonth += this->QnHr(J);
        }
        SumQnMonth /= hrsPerMonth;
        this->QnMonthlyAgg(MonthNum) = SumQnMonth;
    }
    if (this->prevHour != state.dataGroundHeatExchanger->locHourOfDay) {
        this->prevHour = state.dataGroundHeatExchanger->locHourOfDay;
    }
}

//******************************************************************************

void GetGroundHeatExchangerInput(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Dan Fisher
    //       DATE WRITTEN:    August, 2000
    //       MODIFIED         Arun Murugappan
    //       RE-ENGINEERED    na

    bool errorsFound = false;

    // GET NUMBER OF ALL EQUIPMENT TYPES
    state.dataGroundHeatExchanger->numVerticalGLHEs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "GroundHeatExchanger:System");
    state.dataGroundHeatExchanger->numSlinkyGLHEs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "GroundHeatExchanger:Slinky");
    state.dataGroundHeatExchanger->numVertArray =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "GroundHeatExchanger:Vertical:Array");
    state.dataGroundHeatExchanger->numVertProps =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "GroundHeatExchanger:Vertical:Properties");
    state.dataGroundHeatExchanger->numResponseFactors =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "GroundHeatExchanger:ResponseFactors");
    state.dataGroundHeatExchanger->numSingleBorehole =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "GroundHeatExchanger:Vertical:Single");

    if (state.dataGroundHeatExchanger->numVerticalGLHEs <= 0 && state.dataGroundHeatExchanger->numSlinkyGLHEs <= 0) {
        ShowSevereError(state, "Error processing inputs for GLHE objects");
        ShowContinueError(state, "Simulation indicated these objects were found, but input processor doesn't find any");
        ShowContinueError(state, "Check inputs for GroundHeatExchanger:System and GroundHeatExchanger:Slinky");
        ShowContinueError(state, "Also check plant/branch inputs for references to invalid/deleted objects");
        errorsFound = true;
    }

    if (state.dataGroundHeatExchanger->numVertProps > 0) {

        std::string currObj = "GroundHeatExchanger:Vertical:Properties";

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(currObj);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            ShowSevereError(state,                                                                     // LCOV_EXCL_LINE
                            currObj + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
        }

        auto &instancesValue = instances.value();
        for (auto it = instancesValue.begin(); it != instancesValue.end(); ++it) {
            auto const &instance = it.value();
            auto const &objName = it.key();
            auto const &objNameUC = UtilityRoutines::MakeUPPERCase(objName);
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(currObj, objName);
            std::shared_ptr<GLHEVertProps> thisObj(new GLHEVertProps(state, objNameUC, instance));
            state.dataGroundHeatExchanger->vertPropsVector.push_back(thisObj);
        }
    }

    if (state.dataGroundHeatExchanger->numResponseFactors > 0) {

        std::string currObj = "GroundHeatExchanger:ResponseFactors";

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(currObj);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            ShowSevereError(state,                                                                     // LCOV_EXCL_LINE
                            currObj + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
        }

        auto &instancesValue = instances.value();
        for (auto it = instancesValue.begin(); it != instancesValue.end(); ++it) {
            auto const &instance = it.value();
            auto const &objName = it.key();
            auto const &objNameUC = UtilityRoutines::MakeUPPERCase(objName);
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(currObj, objName);
            std::shared_ptr<GLHEResponseFactors> thisObj(new GLHEResponseFactors(state, objNameUC, instance));
            state.dataGroundHeatExchanger->responseFactorsVector.push_back(thisObj);
        }
    }

    if (state.dataGroundHeatExchanger->numVertArray > 0) {

        std::string currObj = "GroundHeatExchanger:Vertical:Array";

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(currObj);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            ShowSevereError(state,                                                                     // LCOV_EXCL_LINE
                            currObj + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
        }

        auto &instancesValue = instances.value();
        for (auto it = instancesValue.begin(); it != instancesValue.end(); ++it) {
            auto const &instance = it.value();
            auto const &objName = it.key();
            auto const &objNameUC = UtilityRoutines::MakeUPPERCase(objName);
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(currObj, objName);
            std::shared_ptr<GLHEVertArray> thisObj(new GLHEVertArray(state, objNameUC, instance));
            state.dataGroundHeatExchanger->vertArraysVector.push_back(thisObj);
        }
    }

    if (state.dataGroundHeatExchanger->numSingleBorehole > 0) {

        std::string currObj = "GroundHeatExchanger:Vertical:Single";

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(currObj);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            ShowSevereError(state,                                                                     // LCOV_EXCL_LINE
                            currObj + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
        }

        auto &instancesValue = instances.value();
        for (auto it = instancesValue.begin(); it != instancesValue.end(); ++it) {
            auto const &instance = it.value();
            auto const &objName = it.key();
            auto const &objNameUC = UtilityRoutines::MakeUPPERCase(objName);
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(currObj, objName);
            std::shared_ptr<GLHEVertSingle> thisObj(new GLHEVertSingle(state, objNameUC, instance));
            state.dataGroundHeatExchanger->singleBoreholesVector.push_back(thisObj);
        }
    }

    if (state.dataGroundHeatExchanger->numVerticalGLHEs > 0) {

        std::string currObj = "GroundHeatExchanger:System";

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(currObj);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            ShowSevereError(state,                                                                     // LCOV_EXCL_LINE
                            currObj + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
        }

        auto &instancesValue = instances.value();
        for (auto it = instancesValue.begin(); it != instancesValue.end(); ++it) {
            auto const &instance = it.value();
            auto const &objName = it.key();
            auto const &objNameUC = UtilityRoutines::MakeUPPERCase(objName);
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(currObj, objName);
            state.dataGroundHeatExchanger->verticalGLHE.emplace_back(state, objNameUC, instance);
        }
    }

    // SLINKY GLHE

    if (state.dataGroundHeatExchanger->numSlinkyGLHEs > 0) {

        std::string currObj = "GroundHeatExchanger:Slinky";

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(currObj);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            ShowSevereError(state,                                                                     // LCOV_EXCL_LINE
                            currObj + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
        }

        auto &instancesValue = instances.value();
        for (auto it = instancesValue.begin(); it != instancesValue.end(); ++it) {
            auto const &instance = it.value();
            auto const &objName = it.key();
            auto const &objNameUC = UtilityRoutines::MakeUPPERCase(objName);
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(currObj, objName);
            state.dataGroundHeatExchanger->slinkyGLHE.emplace_back(state, objNameUC, instance);
        }
    }
}

//******************************************************************************

void GLHEBase::setupOutput(EnergyPlusData &state)
{
    SetupOutputVariable(
        state, "Ground Heat Exchanger Average Borehole Temperature", OutputProcessor::Unit::C, this->bhTemp, "System", "Average", this->name);
    SetupOutputVariable(state, "Ground Heat Exchanger Heat Transfer Rate", OutputProcessor::Unit::W, this->QGLHE, "System", "Average", this->name);
    SetupOutputVariable(state, "Ground Heat Exchanger Inlet Temperature", OutputProcessor::Unit::C, this->inletTemp, "System", "Average", this->name);
    SetupOutputVariable(
        state, "Ground Heat Exchanger Outlet Temperature", OutputProcessor::Unit::C, this->outletTemp, "System", "Average", this->name);
    SetupOutputVariable(
        state, "Ground Heat Exchanger Mass Flow Rate", OutputProcessor::Unit::kg_s, this->massFlowRate, "System", "Average", this->name);
    SetupOutputVariable(
        state, "Ground Heat Exchanger Average Fluid Temperature", OutputProcessor::Unit::C, this->aveFluidTemp, "System", "Average", this->name);
    SetupOutputVariable(
        state, "Ground Heat Exchanger Farfield Ground Temperature", OutputProcessor::Unit::C, this->tempGround, "System", "Average", this->name);
}

//******************************************************************************

Real64 GLHEVert::calcBHAverageResistance(EnergyPlusData &state)
{
    // Calculates the average thermal resistance of the borehole using the first-order multipole method.

    // Javed, S. & Spitler, J.D. 2016. 'Accuracy of Borehole Thermal Resistance Calculation Methods
    // for Grouted Single U-tube Ground Heat Exchangers.' Applied Energy.187:790-806.

    // Equation 13

    Real64 const beta = 2 * DataGlobalConstants::Pi * this->grout.k * calcPipeResistance(state);

    Real64 const final_term_1 = log(this->theta_2 / (2 * this->theta_1 * pow(1 - pow_4(this->theta_1), this->sigma)));
    Real64 const num_final_term_2 = pow_2(this->theta_3) * pow_2(1 - (4 * this->sigma * pow_4(this->theta_1)) / (1 - pow_4(this->theta_1)));
    Real64 const den_final_term_2_pt_1 = (1 + beta) / (1 - beta);
    Real64 const den_final_term_2_pt_2 = pow_2(this->theta_3) * (1 + (16 * this->sigma * pow_4(this->theta_1)) / pow_2(1 - pow_4(this->theta_1)));
    Real64 const den_final_term_2 = den_final_term_2_pt_1 + den_final_term_2_pt_2;
    Real64 const final_term_2 = num_final_term_2 / den_final_term_2;

    return (1 / (4 * DataGlobalConstants::Pi * this->grout.k)) * (beta + final_term_1 - final_term_2);
}

//******************************************************************************

Real64 GLHEVert::calcBHTotalInternalResistance(EnergyPlusData &state)
{
    // Calculates the total internal thermal resistance of the borehole using the first-order multipole method.

    // Javed, S. & Spitler, J.D. 2016. 'Accuracy of Borehole Thermal Resistance Calculation Methods
    // for Grouted Single U-tube Ground Heat Exchangers.' Applied Energy. 187:790-806.

    // Equation 26

    Real64 beta = 2 * DataGlobalConstants::Pi * this->grout.k * calcPipeResistance(state);

    Real64 final_term_1 = log(pow(1 + pow_2(this->theta_1), this->sigma) / (this->theta_3 * pow(1 - pow_2(this->theta_1), this->sigma)));
    Real64 num_term_2 = pow_2(this->theta_3) * pow_2(1 - pow_4(this->theta_1) + 4 * this->sigma * pow_2(this->theta_1));
    Real64 den_term_2_pt_1 = (1 + beta) / (1 - beta) * pow_2(1 - pow_4(this->theta_1));
    Real64 den_term_2_pt_2 = pow_2(this->theta_3) * pow_2(1 - pow_4(this->theta_1));
    Real64 den_term_2_pt_3 = 8 * this->sigma * pow_2(this->theta_1) * pow_2(this->theta_3) * (1 + pow_4(this->theta_1));
    Real64 den_term_2 = den_term_2_pt_1 - den_term_2_pt_2 + den_term_2_pt_3;
    Real64 final_term_2 = num_term_2 / den_term_2;

    return (1 / (DataGlobalConstants::Pi * this->grout.k)) * (beta + final_term_1 - final_term_2);
}

//******************************************************************************

Real64 GLHEVert::calcBHGroutResistance(EnergyPlusData &state)
{
    // Calculates grout resistance. Use for validation.

    // Javed, S. & Spitler, J.D. 2016. 'Accuracy of Borehole Thermal Resistance Calculation Methods
    // for Grouted Single U-tube Ground Heat Exchangers.' Applied Energy. 187:790-806.

    // Equation 3

    return calcBHAverageResistance(state) - calcPipeResistance(state) / 2.0;
}

//******************************************************************************

Real64 GLHEVert::calcHXResistance(EnergyPlusData &state)
{
    // Calculates the effective thermal resistance of the borehole assuming a uniform heat flux.

    // Javed, S. & Spitler, J.D. Calculation of Borehole Thermal Resistance. In 'Advances in
    // Ground-Source Heat Pump Systems,' pp. 84. Rees, S.J. ed. Cambridge, MA. Elsevier Ltd. 2016.

    // Eq: 3-67

    using FluidProperties::GetSpecificHeatGlycol;

    constexpr const char *RoutineName("calcBHResistance");

    if (this->massFlowRate <= 0.0) {
        return 0;
    } else {
        Real64 const cpFluid = GetSpecificHeatGlycol(state,
                                                     state.dataPlnt->PlantLoop(this->loopNum).FluidName,
                                                     this->inletTemp,
                                                     state.dataPlnt->PlantLoop(this->loopNum).FluidIndex,
                                                     RoutineName);
        return calcBHAverageResistance(state) +
               1 / (3 * calcBHTotalInternalResistance(state)) * pow_2(this->bhLength / (this->massFlowRate * cpFluid));
    }
}

//******************************************************************************

Real64 GLHEVert::calcPipeConductionResistance()
{
    // Calculates the thermal resistance of a pipe, in [K/(W/m)].

    // Javed, S. & Spitler, J.D. 2016. 'Accuracy of Borehole Thermal Resistance Calculation Methods
    // for Grouted Single U-tube Ground Heat Exchangers.' Applied Energy. 187:790-806.

    return log(this->pipe.outDia / this->pipe.innerDia) / (2 * DataGlobalConstants::Pi * this->pipe.k);
}

//******************************************************************************

Real64 GLHEVert::calcPipeConvectionResistance(EnergyPlusData &state)
{
    // Calculates the convection resistance using Gnielinski and Petukov, in [K/(W/m)]

    // Gneilinski, V. 1976. 'New equations for heat and mass transfer in turbulent pipe and channel flow.'
    // International Chemical Engineering 16(1976), pp. 359-368.

    using FluidProperties::GetConductivityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;
    using FluidProperties::GetViscosityGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    constexpr const char *RoutineName("calcPipeConvectionResistance");

    // Get fluid props
    this->inletTemp = state.dataLoopNodes->Node(this->inletNodeNum).Temp;

    Real64 const cpFluid = GetSpecificHeatGlycol(
        state, state.dataPlnt->PlantLoop(this->loopNum).FluidName, this->inletTemp, state.dataPlnt->PlantLoop(this->loopNum).FluidIndex, RoutineName);
    Real64 const kFluid = GetConductivityGlycol(
        state, state.dataPlnt->PlantLoop(this->loopNum).FluidName, this->inletTemp, state.dataPlnt->PlantLoop(this->loopNum).FluidIndex, RoutineName);
    Real64 const fluidViscosity = GetViscosityGlycol(
        state, state.dataPlnt->PlantLoop(this->loopNum).FluidName, this->inletTemp, state.dataPlnt->PlantLoop(this->loopNum).FluidIndex, RoutineName);

    // Smoothing fit limits
    constexpr Real64 lower_limit = 2000;
    constexpr Real64 upper_limit = 4000;

    Real64 const bhMassFlowRate = this->massFlowRate / this->myRespFactors->numBoreholes;
    Real64 const reynoldsNum = 4 * bhMassFlowRate / (fluidViscosity * DataGlobalConstants::Pi * this->pipe.innerDia);

    Real64 nusseltNum = 0.0;
    if (reynoldsNum < lower_limit) {
        nusseltNum = 4.01; // laminar mean(4.36, 3.66)
    } else if (lower_limit <= reynoldsNum && reynoldsNum < upper_limit) {
        Real64 const nu_low = 4.01;                   // laminar
        Real64 const f = frictionFactor(reynoldsNum); // turbulent
        Real64 const prandtlNum = (cpFluid * fluidViscosity) / (kFluid);
        Real64 const nu_high = (f / 8) * (reynoldsNum - 1000) * prandtlNum / (1 + 12.7 * std::sqrt(f / 8) * (pow(prandtlNum, 2.0 / 3.0) - 1));
        Real64 const sf = 1 / (1 + std::exp(-(reynoldsNum - 3000) / 150.0)); // smoothing function
        nusseltNum = (1 - sf) * nu_low + sf * nu_high;
    } else {
        Real64 const f = frictionFactor(reynoldsNum);
        Real64 const prandtlNum = (cpFluid * fluidViscosity) / (kFluid);
        nusseltNum = (f / 8) * (reynoldsNum - 1000) * prandtlNum / (1 + 12.7 * std::sqrt(f / 8) * (pow(prandtlNum, 2.0 / 3.0) - 1));
    }

    Real64 h = nusseltNum * kFluid / this->pipe.innerDia;

    return 1 / (h * DataGlobalConstants::Pi * this->pipe.innerDia);
}

//******************************************************************************

Real64 GLHEVert::frictionFactor(Real64 const reynoldsNum)
{
    // Calculates the friction factor in smooth tubes

    // Petukov, B.S. 1970. 'Heat transfer and friction in turbulent pipe flow with variable physical properties.'
    // In Advances in Heat Transfer, ed. T.F. Irvine and J.P. Hartnett, Vol. 6. New York Academic Press.

    // limits picked be within about 1% of actual values
    constexpr Real64 lower_limit = 1500;
    constexpr Real64 upper_limit = 5000;

    if (reynoldsNum < lower_limit) {
        return 64.0 / reynoldsNum; // pure laminar flow
    } else if (lower_limit <= reynoldsNum && reynoldsNum < upper_limit) {
        // pure laminar flow
        Real64 const f_low = 64.0 / reynoldsNum;
        // pure turbulent flow
        Real64 const f_high = pow(0.79 * log(reynoldsNum) - 1.64, -2.0);
        Real64 const sf = 1 / (1 + exp(-(reynoldsNum - 3000.0) / 450.0)); // smoothing function
        return (1 - sf) * f_low + sf * f_high;
    } else {
        return pow(0.79 * log(reynoldsNum) - 1.64, -2.0); // pure turbulent flow
    }
}

//******************************************************************************

Real64 GLHEVert::calcPipeResistance(EnergyPlusData &state)
{
    // Calculates the combined conduction and convection pipe resistance

    // Javed, S. & Spitler, J.D. 2016. 'Accuracy of Borehole Thermal Resistance Calculation Methods
    // for Grouted Single U-tube Ground Heat Exchangers.' J. Energy Engineering. Draft in progress.

    // Equation 3

    return calcPipeConductionResistance() + calcPipeConvectionResistance(state);
}

//******************************************************************************

Real64 GLHESlinky::calcHXResistance(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Matt Mitchell
    //       DATE WRITTEN   February, 2015
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS SUBROUTINE:
    //    Calculates the resistance of the slinky HX from the fluid to the
    //    outer tube wall.

    using FluidProperties::GetConductivityGlycol;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;
    using FluidProperties::GetViscosityGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    constexpr const char *RoutineName("CalcSlinkyGroundHeatExchanger");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 nusseltNum;
    Real64 Rconv;
    constexpr Real64 A(3150);
    constexpr Real64 B(350);
    constexpr Real64 laminarNusseltNo(4.364);

    Real64 cpFluid = GetSpecificHeatGlycol(
        state, state.dataPlnt->PlantLoop(this->loopNum).FluidName, this->inletTemp, state.dataPlnt->PlantLoop(this->loopNum).FluidIndex, RoutineName);
    Real64 kFluid = GetConductivityGlycol(
        state, state.dataPlnt->PlantLoop(this->loopNum).FluidName, this->inletTemp, state.dataPlnt->PlantLoop(this->loopNum).FluidIndex, RoutineName);
    Real64 fluidDensity = GetDensityGlycol(
        state, state.dataPlnt->PlantLoop(this->loopNum).FluidName, this->inletTemp, state.dataPlnt->PlantLoop(this->loopNum).FluidIndex, RoutineName);
    Real64 fluidViscosity = GetViscosityGlycol(
        state, state.dataPlnt->PlantLoop(this->loopNum).FluidName, this->inletTemp, state.dataPlnt->PlantLoop(this->loopNum).FluidIndex, RoutineName);

    // calculate mass flow rate
    Real64 singleSlinkyMassFlowRate = this->massFlowRate / this->numTrenches;

    Real64 pipeInnerRad = this->pipe.outRadius - this->pipe.thickness;
    Real64 pipeInnerDia = 2.0 * pipeInnerRad;

    if (singleSlinkyMassFlowRate == 0.0) {
        Rconv = 0.0;
    } else {
        // Re=Rho*V*D/Mu
        Real64 reynoldsNum = fluidDensity * pipeInnerDia *
                             (singleSlinkyMassFlowRate / fluidDensity / (DataGlobalConstants::Pi * pow_2(pipeInnerRad))) / fluidViscosity;
        Real64 prandtlNum = (cpFluid * fluidViscosity) / (kFluid);
        //   Convection Resistance
        if (reynoldsNum <= 2300) {
            nusseltNum = laminarNusseltNo;
        } else if (reynoldsNum > 2300 && reynoldsNum <= 4000) {
            Real64 sf = 0.5 + 0.5 * std::tanh((reynoldsNum - A) / B);
            Real64 turbulentNusseltNo = 0.023 * std::pow(reynoldsNum, 0.8) * std::pow(prandtlNum, 0.35);
            nusseltNum = laminarNusseltNo * (1 - sf) + turbulentNusseltNo * sf;
        } else {
            nusseltNum = 0.023 * std::pow(reynoldsNum, 0.8) * std::pow(prandtlNum, 0.35);
        }
        Real64 hci = nusseltNum * kFluid / pipeInnerDia;
        Rconv = 1.0 / (2.0 * DataGlobalConstants::Pi * pipeInnerDia * hci);
    }

    //   Conduction Resistance
    Real64 Rcond = std::log(this->pipe.outRadius / pipeInnerRad) / (2.0 * DataGlobalConstants::Pi * this->pipe.k) / 2.0; // pipe in parallel so /2

    return Rcond + Rconv;
}

//******************************************************************************

Real64 GLHEBase::interpGFunc(Real64 const LnTTsVal // The value of LN(t/TimeSS) that a g-function
) const
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Chris L. Marshall, Jeffrey D. Spitler
    //       DATE WRITTEN   1993
    //       MODIFIED       August, 2000
    //       RE-ENGINEERED Dan Fisher

    // PURPOSE OF THIS SUBROUTINE:
    //    To interpolate or extrapolate data in GFILE
    //    to find the correct g-function value for a
    //    known value of the natural log of (T/Ts)

    //  REFERENCE:          Thermal Analysis of Heat Extraction
    //                      Boreholes.  Per Eskilson, Dept. of
    //                      Mathematical Physics, University of
    //                      Lund, Sweden, June 1987.

    // SUBROUTINE ARGUMENT DEFINITIONS:
    //          needs to be found for.
    //          either extrapolation or interpolation

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 gFuncVal;

    // Binary Search Algorithms Variables
    // REFERENCE      :  DATA STRUCTURES AND ALGORITHM ANALYSIS IN C BY MARK ALLEN WEISS

    // The following IF loop determines the g-function for the case
    // when LnTTsVal is less than the first element of the LnTTs array.
    // In this case, the g-function must be found by extrapolation.

    if (LnTTsVal <= this->myRespFactors->LNTTS(1)) {
        gFuncVal = ((LnTTsVal - this->myRespFactors->LNTTS(1)) / (this->myRespFactors->LNTTS(2) - this->myRespFactors->LNTTS(1))) *
                       (this->myRespFactors->GFNC(2) - this->myRespFactors->GFNC(1)) +
                   this->myRespFactors->GFNC(1);
        return gFuncVal;
    }

    // The following IF loop determines the g-function for the case
    // when LnTTsVal is greater than the last element of the LnTTs array.
    // In this case, the g-function must be found by extrapolation.

    int NPairs = this->myRespFactors->LNTTS.u1();

    if (LnTTsVal > this->myRespFactors->LNTTS(NPairs)) {
        gFuncVal = ((LnTTsVal - this->myRespFactors->LNTTS(NPairs)) / (this->myRespFactors->LNTTS(NPairs - 1) - this->myRespFactors->LNTTS(NPairs))) *
                       (this->myRespFactors->GFNC(NPairs - 1) - this->myRespFactors->GFNC(NPairs)) +
                   this->myRespFactors->GFNC(NPairs);
        return gFuncVal;
    }

    // The following DO loop is for the case when LnTTsVal falls within
    // the first and last elements of the LnTTs array, or is identically
    // equal to one of the LnTTs elements.  In this case the g-function
    // must be found by interpolation.
    // USING BINARY SEARCH TO FIND THE ELEMENT
    bool Found = false;
    int Low = 1;
    int High = NPairs;
    int Mid;
    while (Low <= High) {
        Mid = (Low + High) / 2;
        if (this->myRespFactors->LNTTS(Mid) < LnTTsVal) {
            Low = Mid + 1;
        } else {
            if (this->myRespFactors->LNTTS(Mid) > LnTTsVal) {
                High = Mid - 1;
            } else {
                Found = true;
                break;
            }
        }
    }
    // LnTTsVal is identical to one of the LnTTS array elements return gFuncVal
    // the gFuncVal after applying the correction
    if (Found) {
        gFuncVal = this->myRespFactors->GFNC(Mid);
        return gFuncVal;
    }

    // LnTTsVal is in between any of the two LnTTS array elements find the
    // g-function value by interpolation and apply the correction and return gFuncVal
    else {
        if (this->myRespFactors->LNTTS(Mid) < LnTTsVal) ++Mid;

        gFuncVal = ((LnTTsVal - this->myRespFactors->LNTTS(Mid)) / (this->myRespFactors->LNTTS(Mid - 1) - this->myRespFactors->LNTTS(Mid))) *
                       (this->myRespFactors->GFNC(Mid - 1) - this->myRespFactors->GFNC(Mid)) +
                   this->myRespFactors->GFNC(Mid);

        return gFuncVal;
    }
}

//******************************************************************************

Real64 GLHESlinky::getGFunc(Real64 const time)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Gets the g-function for slinky GHXs
    // Note: Base 10 here.

    Real64 LNTTS = std::log10(time);

    return interpGFunc(LNTTS);
}

//******************************************************************************

Real64 GLHEVert::getGFunc(Real64 const time)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Matt Mitchell
    //       DATE WRITTEN:    February, 2015
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Gets the g-function for vertical GHXs
    // Note: Base e here.

    Real64 LNTTS = std::log(time);
    Real64 gFuncVal = interpGFunc(LNTTS);
    Real64 RATIO = this->bhRadius / this->bhLength;

    if (RATIO != this->myRespFactors->gRefRatio) {
        gFuncVal -= std::log(this->bhRadius / (this->bhLength * this->myRespFactors->gRefRatio));
    }

    return gFuncVal;
}

//******************************************************************************

void GLHEVert::initGLHESimVars(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Dan Fisher
    //       DATE WRITTEN:    August, 2000
    //       MODIFIED         Arun Murugappan
    //       RE-ENGINEERED    na

    // Using/Aliasing
    using DataPlant::TypeOf_GrndHtExchgSystem;
    using PlantUtilities::RegulateCondenserCompFlowReqOp;
    using PlantUtilities::ScanPlantLoopsForObject;
    using PlantUtilities::SetComponentFlowRate;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 currTime = ((state.dataGlobal->DayOfSim - 1) * 24 + (state.dataGlobal->HourOfDay - 1) +
                       (state.dataGlobal->TimeStep - 1) * state.dataGlobal->TimeStepZone + state.dataHVACGlobal->SysTimeElapsed) *
                      DataGlobalConstants::SecInHour;

    // Init more variables
    if (this->myFlag) {
        // Locate the hx on the plant loops for later usage
        bool errFlag = false;
        ScanPlantLoopsForObject(
            state, this->name, TypeOf_GrndHtExchgSystem, this->loopNum, this->loopSideNum, this->branchNum, this->compNum, errFlag, _, _, _, _, _);
        if (errFlag) {
            ShowFatalError(state, "initGLHESimVars: Program terminated due to previous condition(s).");
        }
        this->myFlag = false;
    }

    if (this->myEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {
        this->initEnvironment(state, currTime);
    }

    // Calculate the average ground temperature over the depth of the borehole
    Real64 minDepth = this->myRespFactors->props->bhTopDepth;
    Real64 maxDepth = this->myRespFactors->props->bhLength + minDepth;
    Real64 oneQuarterDepth = minDepth + (maxDepth - minDepth) * 0.25;
    Real64 halfDepth = minDepth + (maxDepth - minDepth) * 0.5;
    Real64 threeQuarterDepth = minDepth + (maxDepth - minDepth) * 0.75;

    this->tempGround = 0;

    this->tempGround += this->groundTempModel->getGroundTempAtTimeInSeconds(state, minDepth, currTime);
    this->tempGround += this->groundTempModel->getGroundTempAtTimeInSeconds(state, maxDepth, currTime);
    this->tempGround += this->groundTempModel->getGroundTempAtTimeInSeconds(state, oneQuarterDepth, currTime);
    this->tempGround += this->groundTempModel->getGroundTempAtTimeInSeconds(state, halfDepth, currTime);
    this->tempGround += this->groundTempModel->getGroundTempAtTimeInSeconds(state, threeQuarterDepth, currTime);

    this->tempGround /= 5;

    this->massFlowRate =
        RegulateCondenserCompFlowReqOp(state, this->loopNum, this->loopSideNum, this->branchNum, this->compNum, this->designMassFlow);

    SetComponentFlowRate(
        state, this->massFlowRate, this->inletNodeNum, this->outletNodeNum, this->loopNum, this->loopSideNum, this->branchNum, this->compNum);

    // Reset local environment init flag
    if (!state.dataGlobal->BeginEnvrnFlag) this->myEnvrnFlag = true;
}

//******************************************************************************

void GLHEVert::initEnvironment(EnergyPlusData &state, [[maybe_unused]] Real64 const &CurTime)
{

    constexpr const char *RoutineName("initEnvironment");

    this->myEnvrnFlag = false;

    Real64 fluidDensity = FluidProperties::GetDensityGlycol(
        state, state.dataPlnt->PlantLoop(this->loopNum).FluidName, 20.0, state.dataPlnt->PlantLoop(this->loopNum).FluidIndex, RoutineName);
    this->designMassFlow = this->designFlow * fluidDensity;
    PlantUtilities::InitComponentNodes(
        state, 0.0, this->designMassFlow, this->inletNodeNum, this->outletNodeNum, this->loopNum, this->loopSideNum, this->branchNum, this->compNum);

    this->lastQnSubHr = 0.0;
    state.dataLoopNodes->Node(this->inletNodeNum).Temp = this->tempGround;
    state.dataLoopNodes->Node(this->outletNodeNum).Temp = this->tempGround;

    // zero out all history arrays
    this->QnHr = 0.0;
    this->QnMonthlyAgg = 0.0;
    this->QnSubHr = 0.0;
    this->LastHourN = 0;
    state.dataGroundHeatExchanger->prevTimeSteps = 0.0;
    state.dataGroundHeatExchanger->currentSimTime = 0.0;
    this->QGLHE = 0.0;
    this->prevHour = 1;
}

//******************************************************************************

void GLHESlinky::initGLHESimVars(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Dan Fisher
    //       DATE WRITTEN:    August, 2000
    //       MODIFIED         Arun Murugappan
    //       RE-ENGINEERED    na

    // Using/Aliasing
    using DataPlant::TypeOf_GrndHtExchgSlinky;
    using PlantUtilities::RegulateCondenserCompFlowReqOp;
    using PlantUtilities::ScanPlantLoopsForObject;
    using PlantUtilities::SetComponentFlowRate;
    using namespace GroundTemperatureManager;

    Real64 CurTime = ((state.dataGlobal->DayOfSim - 1) * 24 + (state.dataGlobal->HourOfDay - 1) +
                      (state.dataGlobal->TimeStep - 1) * state.dataGlobal->TimeStepZone + state.dataHVACGlobal->SysTimeElapsed) *
                     DataGlobalConstants::SecInHour;

    // Init more variables
    if (this->myFlag) {
        // Locate the hx on the plant loops for later usage
        bool errFlag = false;
        ScanPlantLoopsForObject(
            state, this->name, TypeOf_GrndHtExchgSlinky, this->loopNum, this->loopSideNum, this->branchNum, this->compNum, errFlag, _, _, _, _, _);
        if (errFlag) {
            ShowFatalError(state, "initGLHESimVars: Program terminated due to previous condition(s).");
        }
        this->myFlag = false;
    }

    if (this->myEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {
        this->initEnvironment(state, CurTime);
    }

    this->tempGround = this->groundTempModel->getGroundTempAtTimeInSeconds(state, this->coilDepth, CurTime);

    this->massFlowRate =
        RegulateCondenserCompFlowReqOp(state, this->loopNum, this->loopSideNum, this->branchNum, this->compNum, this->designMassFlow);

    SetComponentFlowRate(
        state, this->massFlowRate, this->inletNodeNum, this->outletNodeNum, this->loopNum, this->loopSideNum, this->branchNum, this->compNum);

    // Reset local environment init flag
    if (!state.dataGlobal->BeginEnvrnFlag) this->myEnvrnFlag = true;
}

//******************************************************************************

void GLHESlinky::initEnvironment(EnergyPlusData &state, Real64 const &CurTime)
{

    constexpr const char *RoutineName("initEnvironment");

    this->myEnvrnFlag = false;

    Real64 fluidDensity = FluidProperties::GetDensityGlycol(
        state, state.dataPlnt->PlantLoop(this->loopNum).FluidName, 20.0, state.dataPlnt->PlantLoop(this->loopNum).FluidIndex, RoutineName);
    this->designMassFlow = this->designFlow * fluidDensity;
    PlantUtilities::InitComponentNodes(
        state, 0.0, this->designMassFlow, this->inletNodeNum, this->outletNodeNum, this->loopNum, this->loopSideNum, this->branchNum, this->compNum);

    this->lastQnSubHr = 0.0;
    state.dataLoopNodes->Node(this->inletNodeNum).Temp = this->groundTempModel->getGroundTempAtTimeInSeconds(state, this->coilDepth, CurTime);
    state.dataLoopNodes->Node(this->outletNodeNum).Temp = this->groundTempModel->getGroundTempAtTimeInSeconds(state, this->coilDepth, CurTime);

    // zero out all history arrays
    this->QnHr = 0.0;
    this->QnMonthlyAgg = 0.0;
    this->QnSubHr = 0.0;
    this->LastHourN = 0;
    state.dataGroundHeatExchanger->prevTimeSteps = 0.0;
    state.dataGroundHeatExchanger->currentSimTime = 0.0;
    this->QGLHE = 0.0;
    this->prevHour = 1;
}

//******************************************************************************

} // namespace EnergyPlus::GroundHeatExchangers
