NFP Waterside Economizer
================

**R. Raustad, FSEC/UCF**

 - Original Date - 1/13/21
 - Revision Date - 1/20/21 to address comments

**B. Nigusse, FSEC/UCF**
 - Revision Date - 4/13/21 expand design document and scope review


**Task Description**:

**Ability to properly model and control water-side economizers for self contained units** - Ability to model water side economizer in AirloopHVAC:UnitarySystem which enables free cooling and switching of DX compressor in that object.
## Justification for New Feature ##


Manufacturers have a configuration option that includes free cooling using a cooling coil dedicated to waterside economizing connected to the condenser plant (e.g., cooling towers). The cooling coil is upstream of a DX cooling coil which reduces the DX compressor energy use in cooling mode. EnergyPlus does not currently have a mechanism to properly model control of this configuration.

## E-mail and  Conference Call Conclusions ##

Brent 1/15/21

I would suggest to change CoilSystem:Cooling:DX to CoilSystem:Cooling and allow that parent to use chilled water coils.  Seems hard to justify yet-another-coil-system object.  Or add the controls needed to unitary system and model with two unitary objects in series.

MJWitte 1/19/21

1. I like adding CoilSystem:Cooling:Water because it can be used for more than this application.
2. In theory, one could use AirloopHVAC:UnitarySystem with just one coil and no fan, but it still wouldn't have the extra control offset field, and it's become so large that a focused object makes more sense.
3. One could overthink (not me, of course) the naming and think about how all of the existing CoilSystem objects relate to each other. Some of them have controls in them, some do not (*:HeatExchangerAssisted).
4. These all started with CoilSystem:Cooling:DX because we needed a way to control a dx coil that was loose on the branch. Using CoilsSystem for this was perhaps a confusing choice because of the overlap with CoilSystem:*:HeatExchangerAssisted.
5. Some consolidation might be nice, but why? Look at how hard it's been to accomplish that with the DX coils and UnitarySystems. Oh, right, we haven't. So, perhaps better to keep the set of objects and move towards a common code base for them (i.e., they're all part of the same class).
6. So, I guess I'm convincing myself that the proposed object name is ok as-is.
7. I'm fine with leaving off the sensor node.
8. The MultiMode dehumidification control maybe should have a different name. It's only for activating an HXassisted coil, right?

RR: Yes, MultiMode would be used for the HX. If that choice were used the HX would cycle on based on need, otherwise it would always be on. HeatExchangerControlled may be more accurate.

MJWitte: Or HeatExchangerControl or ActivateHeatExchanger. I think including HeatExchanger in the option name is good. If some other way of controlling humidity comes along then a new option can be added.


## Overview ##

The waterside economizer is used as a pre-cooling coil in packaged DX equipment.

**The following description was excerpt from Trane Product Catalog PKG-PRC024D-EN**.

The waterside economizer substantially reduces the compressor energy requirements because
it uses the cooling water before it enters the condensers. Disadvantages include higher airside pressure drop and a higher head on condenser water pumps.

![Figure 1 Schematic of waterside economizer](NFP-Waterside-Economizer-Fig01.PNG)

**Figure 1 Schematic of waterside economizer (Extracted from Trane Product Catalog)**


**Unit Operating Limits**

**Airflow**:
The minimum recommended airflow for proper VAV system staging and temperature control is 35 percent of nominal design airflow. Adjusting VAV boxes with the appropriate minimum settings prevents the self-contained unit from operating in a surge condition at airflows below this point. Continuous operation in a surge condition can cause fan failure.
Note: Contact your local Trane sales office for minimum airflow conditions.

**Water Flow**: Use 3 gpm/ton for optimum unit capacity and efficiency. Use 2.5 or 2 gpm/ton to reduce pump energy, cooling tower, and piping costs. However, these reduced water flows may impact unit capacity and efficiency by one or two percent. Consult factory for unit specific water flow ranges.

**Supply Air Temperature Control Unit Sequence of Operation
1** — Occupied Supply Air Temperature Control Cooling/Waterside Economizer
During occupied cooling mode, the waterside economizer option and mechanical cooling are used to control the supply air temperature. The supply air temperature setpoint is user defined.
After the fans run for 5 minutes, and a call for cooling is calculated, the Cooling Tower/Pump Command is turned on. After the Condenser Water flow switch closes proving flow, the
Compressors and/or Economizer will be allowed to start.
Waterside economizing enables when the unit’s entering water temperature is less than the
entering mixed air temperature. This is set at Waterside Economizer Enable Offset setpoint. The factory default is 7°F, but can be adjusted by the user.

The economizer acts as the first stage of cooling. If the economizer is unable to maintain the supply air setpoint, the unit controller will bring on the compressors as required to meet the setpoint. If the unit does not include an economizer, only mechanical cooling will satisfy cooling requirements.

**Discharge Air Cooling Setpoint** The Discharge Air Cooling Setpoint default valve of 55 degrees and the unit will control to this value when in the cooling mode. The setpoint can be adjusted 3 different ways.

 - Reset Based on Return Air Temperature: The Discharge Air Temperature can be reset based off the return air temperature which will work to keep this temepature between its heating and cooling setpoints.
 - Reset Based on Zone Temperature: The Discharge Air Temperature can be reset based off the return air temperature which will work to keep this temepature between its heating and cooling setpoints.
 - Reset Based on Zone Temperature: Zone reset is applied to the zone(s) in a building that tends to overcool or overheat. The supply air temperature setpoint is adjusted based on the temperature of the critical zone(s).This can have the effect of improving comfort and/or lowering energy usage.The user-defined parameters are the same as for outdoor air reset.

**End Trane Product Catalog excerpt**.


**Note**: The set point control methods will remain the responsibility of existing EnergyPlus SetPointManagers.

## Approach ##

The waterside economizer is simply a water coil with special controls. The coil is not active if the entering coil air temperature is not sufficiently higher than the entering water temperature by some user specified value (e.g., 7 °F or 3.89 °C) and is controlled to a set point temperature. These controls must be "actuated" by some means. Since the existing UnitarySystem already has 3 coils within the parent, the initial  proposal is to wrap the existing water cooling coils in a parent object and place this new object upstream of the UnitarySystem to achieve the goal of free cooling. There are several coil wrappers in EnergyPlus as follows:

    CoilSystem:Cooling:DX
    A6,  \field Cooling Coil Object Type
         \key Coil:Cooling:DX:SingleSpeed
         \key CoilSystem:Cooling:DX:HeatExchangerAssisted
         \key Coil:Cooling:DX:TwoSpeed
         \key Coil:Cooling:DX:TwoStageWithHumidityControlMode
         \key Coil:Cooling:DX:VariableSpeed
         \key Coil:Cooling:DX:SingleSpeed:ThermalStorage

    CoilSystem:Heating:DX,
    A3,  \field Heating Coil Object Type
         \key Coil:Heating:DX:SingleSpeed
         \key Coil:Heating:DX:VariableSpeed

    CoilSystem:Cooling:Water:HeatExchangerAssisted,
    A2 , \field Heat Exchanger Object Type
         \key HeatExchanger:AirToAir:FlatPlate
         \key HeatExchanger:AirToAir:SensibleAndLatent
    A4 , \field Cooling Coil Object Type
         \key Coil:Cooling:Water
         \key Coil:Cooling:Water:DetailedGeometry

    CoilSystem:Cooling:DX:HeatExchangerAssisted,
    A2 , \field Heat Exchanger Object Type
         \key HeatExchanger:AirToAir:FlatPlate
         \key HeatExchanger:AirToAir:SensibleAndLatent
         \key HeatExchanger:Desiccant:BalancedFlow
    A4 , \field Cooling Coil Object Type
         \key Coil:Cooling:DX:SingleSpeed
         \key Coil:Cooling:DX:VariableSpeed

It may be time to begin consolodating these exising wrappers as done with the Coil:Cooling:DX object. The name would simply be CoilSystem and contain any type of coil with an optional heat exchangera available for cooling coils. This big of a change may be too big for this task, however, keeping this in mind for this new object is highly desireable.

**Instead of incorporating a waterside economizer coil object into a packaged system, a generic method of providing free cooling is discussed here. The new object would be configured as a condenser plant demand side component and placed upstream of any existing coil types. A new wrapper will be developed to include the water coils. This wrapper could be specific to a new waterside economizer coil object or be more generic to allow branch water coils to be simulated without using the Controller:WaterCoil object. The more generic method is described here.**

**Note**: This new object may also be used on the demand side of a cooling plant to support existing coil types.

## Input Description ##


    CoilSystem:Cooling:Water
        \memo Virtual container component that consists of a water cooling coil
        \memo and its associated controls. This control object supports the
        \memo available water coil types and may be placed directly on an
        \memo air loop branch or in an outdoor air equipment list.
        \min-fields 6
    A1, \field Name
        \required-field
        \reference WaterCoilSystemName
        \type alpha
        \reference-class-name validBranchEquipmentTypes
        \reference validBranchEquipmentNames
        \reference-class-name validOASysEquipmentTypes
        \reference validOASysEquipmentNames
    A2, \field Cooling Coil System Inlet Node Name
        \required-field
        \type node
    A3, \field Cooling Coil System Outlet Node Name
        \required-field
        \type node
    A4, \field Availability Schedule Name
        \note Availability schedule name for this system. Schedule value > 0
        \note means the system is available.
        \note If this field is blank, the system is always available.
        \type object-list
        \object-list ScheduleNames
    A5, \field Cooling Coil Object Type
        \type choice
        \required-field
        \key Coil:Cooling:Water
        \key Coil:Cooling:Water:DetailedGeometry
        \key CoilSystem:Cooling:Water:HeatExchangerAssisted ??
    A6, \field Cooling Coil Name
        \required-field
        \type object-list
        \object-list CoolingCoilsWater
    N1, \field Minimum Air To Water Temperature Offset
        \note Coil will turn on as required when inlet air temperature is above
        \note water temperature by amount of offset. To model a waterside
        \note economizer connect to condenser loop and increase offset as desired.
        \type real
        \units C
        \minimum 0.0
        \default 0.0
    A7, \field Dehumidification Control Type
        \type choice
        \key None
        \key HeatExchangerControl
        \key CoolReheat
        \default None
        \note None = meet sensible load only
        \note HeatExchangerControl = activate water coil and meet sensible load.
        \note If no sensible load exists, and Run on Latent Load = Yes, and a latent
        \note load exists, the coil will operate to meet the latent load.
        \note If the latent load cannot be met the heat exchanger will be activated.
        \note IF Run on Latent Load = No, the heat exchanger will always be active.
        \note CoolReheat = cool beyond the dry-bulb setpoint as required
        \note to meet the humidity setpoint.
        \note For all dehumidification controls, the max
        \note humidity setpoint on the Sensor Node is used.
        \note SetpointManager:SingleZone:Humidity:Maximum,
        \note SetpointManager:MultiZone:Humidity:Maximum, or
        \note SetpointManager:MultiZone:MaximumHumidity:Average, and
        \note SetpointManager:OutdoorAirPretreat (optional) objects.
    A8, \field Run on Sensible Load
        \type choice
        \key Yes
        \key No
        \default Yes
        \note If Yes, unit will run if there is a sensible load.
        \note If No, unit will not run if there is only a sensible load.
        \note Dehumidification controls will be active if specified.
    A9; \field Run on Latent Load
        \type choice
        \key Yes
        \key No
        \default No
        \note If Yes, unit will run if there is a latent load.
        \note even if there is no sensible load.
        \note If No, unit will not run only if there is a latent load.
        \note Dehumidification controls will be active if specified.

Several considerations need to be resolved for this new object.

 - Should a Sensor Node Name input field be included, or force use of the object or coil outlet node (recommended and agreed to via comments)
 - Should a controller tolerance input field be included or assume a typical value of 0.001 for temperature control and 0.00001 for humidity control (no comments, assumed OK)
 - If the CoilSystem objects are consolidated, additional inputs would be needed for Use Outdoor Air DX Cooling Coil (A11) and Outdoor Air DX Cooling Coil Leaving Minimum Air Temperature (N1) to replicate all inputs for CoilSystem:Cooling:DX. Additional inputs would also be required for heat exchanger type and name. (discouraged via comments)

## Testing/Validation/Data Sources ##

Compare results of existing coil types with this new object. Also test performance of the new waterside economizer model with object attached to a condenser loop and upstream of a UnitarySystem.

## Input Output Reference Documentation ##

Add new IO section for new object.

## Outputs Description ##

	Cooling Coil Water Flow Rate
	Cooling Coil Water Flow Fraction
	Cooling Coil Total Cooling Rate
	Cooling Coil Total Cooling Energy
	Cooling Coil Source Side Heat Transfer Energy
	Cooling Coil Sensible Cooling Rate
	Cooling Coil Sensible Cooling Energy
	Cooling Coil Latent Cooling Rate
	Cooling Coil Latent Cooling Energy
	Cooling Coil Water-Side Economizer Status

## Engineering Reference ##

Add new Engineering section for new object.

## Example File and Transition Changes ##

No transition required. New example file(s) will be included.

## References ##

None.

## Design Document ##

The UnitarySystem is the only air-side parent that uses the pointer/factory method. The pointer of the class HVACSystemData. The same appraoach will be used for CoilWaterSystems.

SimAirServingZones::SimAirLoopComponent

    void SimAirLoopComponent(EnergyPlusData &state,
                             std::string const &CompName,
                             int CompType_Num,
                             bool FirstHVACIteration,
                             int AirLoopNum,       // Primary air loop number
                             int &CompIndex,
                             HVACSystemData *CompPointer); // HVACSystemData pointer


### EnergyPlusData ###
EnergyPlusData


namespace EnergyPlus {

    EnergyPlusData::EnergyPlusData() {
        // todo, try to eliminate the need for the singleton
        IOFiles::setSingleton(&files);

        this->dataAirflowNetworkBalanceManager =
              std::make_unique<AirflowNetworkBalanceManagerData>();
        this->dataAirLoop = std::make_unique<DataAirLoopData>();
        this->dataAirLoopHVACDOAS = std::make_unique<AirLoopHVACDOASData>();
        this->dataAirSystemsData = std::make_unique<AirSystemsData>();

        **New pointer:**

        this->dataCoilWaterSystems = std::make_unique<CoilWaterSystemsData>();
        ...
    } // EnergyPlusData::EnergyPlusData()


    void EnergyPlusData::clear_state() {
        this->dataAirflowNetworkBalanceManager->clear_state();
        this->dataAirLoop->clear_state();
        this->dataAirLoopHVACDOAS->clear_state();
        this->dataAirSystemsData->clear_state();

        **New clear_state call:**

        this->dataCoilWaterSystems->clear_state();
        ...
    }
} // namespace EnergyPlus

### CoilWaterSystems ###
New header file:

CoilWaterSystems.hh

    New classes:

namespace CoilWaterSystems {

    enum class CCoils
    {
        Unassigned,
        Simple,
        Detailed,
        HXAssist
    };

    enum class DehumCtrlType : int
    {
        None,
        CoolReheat,
        HeatExchangerControl
    };

    struct CoilSysCoolingWaterInputSpecification
    {
        std::string name;
        std::string cooling_coil_system_inlet_node_name;
        std::string cooling_coil_system_outlet_node_name;
        std::string availability_schedule_name;
        std::string cooling_coil_object_type;
        std::string cooling_coil_name;
        Real64 minimum_air_to_water_temperature_offset;
        std::string dehumidification_control_type;
        std::string run_on_sensible_load;
        std::string run_on_latent_load;
    };

    struct CoilWaterSys : HVACSystemData
    {
        // member variables
        CoilSysCoolingWaterInputSpecification original_input_specs;

        std::string name;              // name of water coooling coil
        bool myOneTimeInitFlag = true; // executed only once flag
        int coilWaterSysIndex = 0;     // index of coil system cooling water object
        int airInletNodeIndex = 0;     // cooling coil system inlet node name
        int airOutletNodeIndex = 0;    // cooling coil system outlet node name
        int availScheduleIndex = 0;    // index of availability schedule
        CCoils waterCoolingCoilType;   // type of water coooling coil type
        std::string coolingCoilName;   // type of water coooling coil name
        int coolingCoilType_Num = 0;   // water cooling coil type number (Coil_CoolingWater, Coil_CoolingWaterDetailed, CoilWater_CoolingHXAssisted)
        Real64 minAirToWaterTempOffset = 0.0; // entering air to entering water temperature offset
        int coolingCoilIndex = 0;             // index of coil cooling water
        DehumCtrlType dehumControlType;       // dehumidification control type
        bool runOnSensibleLoad = true;        // true if this system runs to meet a sensible load
        bool runOnLatentLoad = false;         // true if this system runs to meet a latent-only load

        bool myPlantScanFlag = true;       // one time plant scan flag
        Real64 maxCoolCoilFluidFlow = 0.0; // maximum coil water flow rate
        int waterInletNodeIndex = 0;       // index of water coil entering node
        int waterOutletNodeIndex = 0;      // index of water coil leaving node
        int coolCoilLoopNum = 0;           // index plant loop (condenser loop)
        int coolCoilLoopSide = 0;          // index plant loop side
        int coolCoilBranchNum = 0;         // index plant loop branch
        int coolCoilCompNum = 0;           // index of water coil on a branch

        // CoilWaterSys() = default;
        // member functions
        static void getInput(EnergyPlusData &state);
        void instantiateFromInputSpec(EnergyPlusData &state, const CoilSysCoolingWaterInputSpecification &input_data);
        void setOutputReportVariables(EnergyPlus::EnergyPlusData &state);
        void initCoilWaterSystems(EnergyPlusData &state, int const AirLoopNum);
        void controlCoilWaterSystems(EnergyPlusData &state, int const AirLoopNum, bool const FirstHVACIteration);
        static Real64 coolWaterTempResidual(EnergyPlusData &state, Real64 const WaterFlowFraction, std::vector<Real64> const &Par);
        static Real64 coolWaterHumRatResidual(EnergyPlusData &state, Real64 const WaterFlowFraction, std::vector<Real64> const &Par);
        static Real64 HXAssistedCoolCoilHRResidual(EnergyPlusData &state, Real64 const WaterFlowFraction, std::vector<Real64> const &Par);
        static Real64 HXAssistedCoolCoilTempResidual(EnergyPlusData &state, Real64 const WaterFlowFraction, std::vector<Real64> const &Par);
        void reportCoilWaterSystems(EnergyPlus::EnergyPlusData &state);

    public:
        // external variables
        // external functions

        CoilWaterSys(); // constructor

        ~CoilWaterSys() // destructor
        {
        }

        static void getInputData(EnergyPlusData &state);

        static HVACSystemData *
            factory(EnergyPlusData &state, int const object_type_num, std::string const objectName, bool const ZoneEquipment, int const ZoneOAUnitNum);

        void simulateSys(EnergyPlusData &state,
                         std::string const &Name,
                         bool const firstHVACIteration,
                         int const &AirLoopNum,
                         int &CompIndex,
                         bool &HeatActive,
                         bool &CoolActive,
                         int const OAUnitNum,         // system is OutdoorAirUnit
                         Real64 const OAUCoilOutTemp, // Tinlet OutdoorAirUnit
                         bool const ZoneEquipment,    // TRUE if zone equipment
                         Real64 &sysOutputProvided,   // supply node sensible output
                         Real64 &latOutputProvided    // supply node latent output
        );

        void simulate(EnergyPlusData &state,
                      std::string const &Name,
                      bool const firstHVACIteration,
                      int const &AirLoopNum,
                      int &CompIndex,
                      bool &HeatActive,
                      bool &CoolActive,
                      int const OAUnitNum,         // system is OutdoorAirUnit
                      Real64 const OAUCoilOutTemp, // Tinlet OutdoorAirUnit
                      bool const ZoneEquipment,    // TRUE if zone equipment
                      Real64 &sysOutputProvided,   // supply node sensible output
                      Real64 &latOutputProvided    // supply node latent output
                      ) override;


    }; // struct CoilWaterSys : HVACSystemData

} // namespace CoilWaterSystems

struct CoilWaterSystemsData : BaseGlobalStruct
{

    std::vector<CoilWaterSystems::CoilWaterSys> coilWaterSys;
    std::string const coilSysCoolingWaterObjectName = "CoilSystem:Cooling:Water";
    int numCoilWaterSystems = 0;
    bool initCoilWaterSystemsErrFlag = true;
    bool coilWaterSystemsGetInputFlag = true;
    int CoolingCoilIndex = 0;          // index of water coooling coil
    int waterInletNodeIndex = 0;       // index of water entering node of a water cooling coil
    bool myOneTimeFlag = true;         // one time flag
    bool mySetPointCheckFlag = true;   // one time setpoint check flag
    bool getInputOnceFlag = true;      // get input flag
    bool economizerFlag = false;       // holds air loop economizer status
    bool waterCoilDisableFlag = false; // true if coil water inlet temp > coil air inlet temp - minus offset
    bool initCoilWaterSysErrFlag = false;
    int const On = 1; // normal water coil operation, always on

    void clear_state() override
    {
        coilWaterSys.clear();
        numCoilWaterSystems = 0;
        coilWaterSystemsGetInputFlag = true;
        initCoilWaterSystemsErrFlag = true;
        CoolingCoilIndex = 0;
        waterInletNodeIndex = 0;
        myOneTimeFlag = true;
        mySetPointCheckFlag = true;
        getInputOnceFlag = true;
        economizerFlag = false;
        waterCoilDisableFlag = false;
        initCoilWaterSysErrFlag = false;
    }

    // Default Constructor
    CoilWaterSystemsData() = default;
};


### SimAirServingZones::GetAirPathData ###
Adds a "CoilSystem:Cooling:Water" coil type and a pointer to the airpath data

SimAirServingZones::GetAirPathData

    } else if (componentType == "COILSYSTEM:COOLING:DX") {
        state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).Branch(BranchNum).
            Comp(CompNum).CompType_Num = DXSystem;
    } else if (componentType == "COILSYSTEM:HEATING:DX") {
        state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).Branch(BranchNum).
            Comp(CompNum).CompType_Num = DXHeatPumpSystem;
    } else if (componentType == "COIL:USERDEFINED") {
        state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).Branch(BranchNum).
            Comp(CompNum).CompType_Num = CoilUserDefined;
    } else if (componentType == "AIRLOOPHVAC:UNITARYSYSTEM") {
        state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).Branch(BranchNum).
            Comp(CompNum).CompType_Num = UnitarySystemModel;
        UnitarySystems::UnitarySys thisSys;
        state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).Branch(BranchNum).
            Comp(CompNum).compPointer =
            thisSys.factory(state,
                            DataHVACGlobals::UnitarySys_AnyCoilType,
                            state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).
                                Branch(BranchNum).Comp(CompNum).Name,
                            false,
                            0);

    **New factory call:**

    } else if (componentType == "COILSYSTEM:COOLING:WATER") {
        state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).Branch(BranchNum).
            Comp(CompNum).CompType_Num = CoilWaterSystem;
        CoilWaterSystems::CoilWaterSys thisSys;
        state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).Branch(BranchNum).
            Comp(CompNum).compPointer =
            thisSys.factory(state,
                            DataHVACGlobals::CoilWaterSys,
                            state.dataAirSystemsData->PrimaryAirSystems(AirSysNum).
                                Branch(BranchNum).Comp(CompNum).Name,
                            false,
                            0);

    } else {}...

### SimAirServingZones::SimAirLoopComponent ###

Adds new simulate call using override for HVACSystemData::simulate:

SimAirServingZones::SimAirLoopComponent
        {
            auto const SELECT_CASE_var(CompType_Num);

            ...

            } else if (SELECT_CASE_var == DXSystem) { // CoilSystem:Cooling:DX'
                SimDXCoolingSystem(state, CompName, FirstHVACIteration,
                                   AirLoopNum, CompIndex, _, _, QActual);
                if (QActual > 0.0) CoolingActive = true; // determine if coil is ON

            } else if (SELECT_CASE_var == DXHeatPumpSystem) {// CoilSystem:Heating:DX
                SimDXHeatPumpSystem(state, CompName, FirstHVACIteration, AirLoopNum,
                                    CompIndex, _, _, QActual);
                if (QActual > 0.0) HeatingActive = true; // determine if coil is ON

            } else if (SELECT_CASE_var == CoilUserDefined) { // Coil:UserDefined
                SimCoilUserDefined(state, CompName, CompIndex, AirLoopNum,
                                   HeatingActive, CoolingActive);

            } else if (SELECT_CASE_var == UnitarySystemModel) {
                // 'AirLoopHVAC:UnitarySystem'
                Real64 sensOut = 0.0;
                Real64 latOut = 0.0;
                CompPointer->simulate(state,
                                      CompName,
                                      FirstHVACIteration,
                                      AirLoopNum,
                                      CompIndex,
                                      HeatingActive,
                                      CoolingActive,
                                      OAUnitNum,
                                      OAUCoilOutTemp,
                                      ZoneEquipFlag,
                                      sensOut,
                                      latOut);

            **New simulate call for CoilWaterSystem using override for HVACSystemData::simulate:**

            } else if (SELECT_CASE_var == CoilWaterSystem) {
                // 'CoilSystem:Cooling:Water'
                Real64 sensOut = 0.0;
                Real64 latOut = 0.0;
                CompPointer->simulate(state,
                                      CompName,
                                      FirstHVACIteration,
                                      AirLoopNum,
                                      CompIndex,
                                      HeatingActive,
                                      CoolingActive,
                                      OAUnitNum,
                                      OAUCoilOutTemp,
                                      ZoneEquipFlag,
                                      sensOut,
                                      latOut);


Add component type for 'CoilSystem:Cooling:Water' in Primary Air Loop
    constexpr int Fan_System_Object(28);
    constexpr int UnitarySystemModel(29);
    constexpr int ZoneVRFasAirLoopEquip(30);

    Add component type here
    constexpr int CoilWaterSystems(31);

### CoilWaterSystems.cc ###
New file:

    void CoilWaterSys::simulate(EnergyPlusData &state,
                                std::string const &Name,
                                bool const FirstHVACIteration,
                                int const &AirLoopNum,
                                int &CompIndex,
                                bool &HeatActive,
                                bool &CoolActive,
                                int const ZoneOAUnitNum,
                                Real64 const OAUCoilOutTemp,
                                bool const ZoneEquipment,
                                Real64 &sysOutputProvided,
                                Real64 &latOutputProvided)
    {


		Get Input Here

	    if (this->myOneTimeInitFlag) {
		    set report variables here
            this->setOutputReportVariables(state);
            this->myOneTimeInitFlag = false;
        }

        init water systems coils
        this->initCoilWaterSystems(state, AirLoopNum, FirstHVACIteration);

		control water systems coils
        this->controlCoilWaterSystems(state, AirLoopNum, FirstHVACIteration);

        report the current output:
        this->reportCoilWaterSystems(state, AirLoopNum);

    }

	void CoilWaterSys::initCoilWaterSystems(EnergyPlusData &state,
	                                        int const AirLoopNum)
	{
		check if there is a setpoint on the coil air outlet node

		get setpoint value each iteration for temperature or humidity ratio setpoints

		initialize cooling coil water flow rates on the demand side of condenser loop
		at the beginning of each environment

		set the waster-side econmizer enable/disbale flag

	}

    void CoilWaterSys::controlWaterCoolingCoil(EnergyPlusData &state,
	                                           int const AirLoopNum,
                                               bool const FirstHVACIteration
    )
	{
	    determines coil water flow rate that meets the temperature or humidity setpoint
	    for each water coil type. Uses regulafolsi to compute coil water flow fraction

		implementation steps:
		(1) check the coil is available and air flow rate is not zero
		(2) check the water-side economizer enabling falg
		(3) check the control type: RunOnSensibleLoad or RunOnLatentLoad
		(4) do calculation for each control type and for each cooling coil type
		(5) First: do temperature control calculation
		(6) check if dehumid control is active and check if desired HumRat is met
		(7) Second: do HumRat control calculation if the desired HumRat is not met
		(8) save output variables

	}

    void CoilWaterSys::setupOutputVariables(EnergyPlus::EnergyPlusData &state)
    {
	    Adds the following report variables

		Cooling Coil Water Flow Rate
		Cooling Coil Water Flow Fraction
		Cooling Coil Total Cooling Rate
		Cooling Coil Total Cooling Energy
		Cooling Coil Source Side Heat Transfer Energy
		Cooling Coil Sensible Cooling Rate
		Cooling Coil Sensible Cooling Energy
	    Cooling Coil Water-Side Economizer Status
	}

	Real64 CoilWaterSys::calcCoilWaterSystemTempResidual(EnergyPlusData &state,
	             Real64 const WaterFlowFraction, std::vector<Real64> const &Par)
	{}

	Real64 CoilWaterSys::calcCoilWaterSystemHumRatResidual(EnergyPlusData &state,
	             Real64 const WaterFlowFraction, std::vector<Real64> const &Par)
	{}

	void CoilWaterSys::reportCoilWaterSystems(EnergyPlusData &state,
	                                          int const AirLoopNum)
    {
	     update report variables for water cooling coils
	}


### DataPlant.cc ###
    Add "CoilSystem:Cooling:Water" system cooling coil to the 1D String lists:

	SimPlantEquipTypes
	ccSimPlantEquipTypes
	ValidLoopEquipTypes

	Change NumSimPlantEquipTypes(96) to NumSimPlantEquipTypes(97);

	Add parameter index for "CoilSystem:Cooling:Water" component
    constexpr int TypeOf_CoilSystemCoolingWater(97);

    Set loop type to "Condenser" for now
	LoopType::Condenser


    for (CompNum = 1; CompNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum)
	                                  .Branch(BranchNum).TotalComponents; ++CompNum) {
					// set up some references
					auto &this_comp_type(CompTypes(CompNum));
					auto &this_comp(state.dataPlnt->PlantLoop(LoopNum)
						  .LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum));
					...

				   } else if (UtilityRoutines::SameString(this_comp_type,
				        "Coil:Cooling:Water")) {
						this_comp.TypeOf_Num = TypeOf_CoilWaterCooling;
						this_comp.CurOpSchemeType = DemandOpSchemeType;
					} else if (UtilityRoutines::SameString(this_comp_type,
			            "Coil:Cooling:Water:DetailedGeometry")) {
						this_comp.TypeOf_Num = TypeOf_CoilWaterDetailedFlatCooling;
						this_comp.CurOpSchemeType = DemandOpSchemeType;
					} else if (UtilityRoutines::SameString(this_comp_type,
					    "Coil:Heating:Water")) {
						this_comp.TypeOf_Num = TypeOf_CoilWaterSimpleHeating;
						this_comp.CurOpSchemeType = DemandOpSchemeType;
					} else if (UtilityRoutines::SameString(this_comp_type,
					    "Coil:Heating:Steam")) {
						this_comp.TypeOf_Num = TypeOf_CoilSteamAirHeating;
						this_comp.CurOpSchemeType = DemandOpSchemeType;

                    **New demand side condenser equipment "CoilSystem:Cooling:Water"**

					} else if (UtilityRoutines::SameString(this_comp_type,
					    "CoilSystem:Cooling:Water")) {
						this_comp.TypeOf_Num = TypeOf_CoilSystemCoolingWater;
						this_comp.CurOpSchemeType = DemandOpSchemeType;
					...



### MixedAir.cc ###

Add similate calling point under SimOAComponent() function

    {
        auto const SELECT_CASE_var(CompTypeNum);

        if (SELECT_CASE_var == OAMixer_Num) { // 'OutdoorAir:Mixer'
            if (Sim) {
                SimOAMixer(state, CompName, FirstHVACIteration, CompIndex);
            }

        ...

        } else if (SELECT_CASE_var == UnitarySystemModel) { // AirLoopHVAC:UnitarySystem
            if (Sim) {
            }
            if (state.dataMixedAir->MyOneTimeCheckUnitarySysFlag(OASysNum)) {
            }

		**New simulate call for CoilWaterSystem**
		} else if (SELECT_CASE_var == CoilWaterSystem) { // // 'CoilSystem:Cooling:Water'

	            if (Sim) {
                bool HeatingActive = false;
                bool CoolingActive = false;
                Real64 OAUCoilOutTemp = 0.0;
                bool ZoneEquipFlag = false;
                Real64 sensOut = 0.0;
                Real64 latOut = 0.0;
                state.dataAirLoop->OutsideAirSys(OASysNum).compPointer[CompIndex]->simulate(state,
                                                                                            CompName,
                                                                                            FirstHVACIteration,
                                                                                            AirLoopNum,
                                                                                            CompIndex,
                                                                                            HeatingActive,
                                                                                            CoolingActive,
                                                                                            CompIndex,
                                                                                            OAUCoilOutTemp,
                                                                                            ZoneEquipFlag,
                                                                                            sensOut,
                                                                                            latOut);
            }

        } else if (SELECT_CASE_var == DXHeatPumpSystem) {
            if (Sim) {
                SimDXHeatPumpSystem(state, CompName, FirstHVACIteration, AirLoopNum, CompIndex);
            }
            OAHeatingCoil = true;
        }

		...

    }

Add component type for 'CoilSystem:Cooling:Water' in Mixed Air
    constexpr int Fan_System_Object(22);
    constexpr int UnitarySystemModel(23);
    constexpr int VRFTerminalUnit(24);

    Add component type here
    constexpr int CoilWaterSystems(25);

