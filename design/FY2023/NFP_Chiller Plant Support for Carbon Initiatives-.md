Chiller Plant Support for Carbon Initiatives
================

**Richard Raustad**

**Florida Solar Energy Center***

 - Original Date: Apr 18, 2023
 - Modified Date:

## Justification for Feature Update

The push for higher HVAC system efficiency and decarbonization has lead to HVAC systems with new designs for the hot and cold water plants that use heat pumps to serve both plants. Manufacturers, building owners and operators, and design professionals need better tools for modeling these advanced plant configurations for todays HVAC systems.

Chilled water plants have traditionally used thermal storage to minimize building peak demand to help utility companies delay construction of new power plants. Current trends are now using thermal storage as a pathway for heat recovery and improved HVAC system plant efficiency. A water plant utilizing heat pumps could make ice during early morning or late evening hours when heating is required and burn that ice during the day to offset cooling loads. These plants would use high efficiency heat pumps and alternate plant configurations that cannot be modeled in current building simulation software tools. A plant design such as this would also require advanced controls to dispatch heat recovery heat pumps, multiple chiller heaters serving the hot and cold water loops, and determine times for ice thermal storage production.

This proposal outlines a new feature for a plant supervisory controller and an expansion of the existing HeatPump:PlantLoop:EIR:Cooling and HeatPump:PlantLoop:EIR:Heating objects to include typical equipment operating features and limitations. The supervisory controller will build on the existing plant control techniques by adding a new object to manage control of the hot and cold water plants and the equipment serving those plants as *PlantEquipmentOperation:ChillerHeaterChangeover*. The existing HeatPump:PlantLoop:EIR objects will be improved to include new features, for example defrost controls, operating temperature limits, and allow controls to meet either a plant load or a chiller heater leaving water set point.

The proposed approach would be to develop the new supervisory controller and add new functionality to the existing chiller heaters such that a plant of this type could be modeled. Added controls and alternate plant designs could be added in the future. 

## Overview ##

HVAC system water plant equipment would typically include a chiller and boiler. Newer plants use heat pumps with diverting values such that one or more chiller heaters could serve either plant (return side diverting valves not shown in figure). A discussion of whether to model diverting valves or rely on the traditional EnergyPlus plant loop topology resulted in the selection of the current plant topology. The fact that this plant configuration would mix plant fluids from both loops will be mentioned here without further discussion.

The new supervisory controller will oversee and dispatch plant equipment by choosing which plant component will operate at any given time using the plant RunFlag variable. Figure 1 shows a traditional plant with 2 heat pumps. Either heat pump can serve either plant using diverting valves. This concept is modeled in EnergyPlus using a pair of HeatPump:PlantLoop:EIR objects, where only 1 of the pair can be on at any given time, and a PlantEquipmentOperation:ChillerHeaterChangeover object to manage operation. The figure also includes optional equipment for an auxiliary boiler, in case the chiller heater temperature limits may not allow operation, and thermal storage for heat recovery or traditional use, and various chiller heater configurations. The heat pumps can be configured as 2-pipe air-cooled, 4-pipe water-cooled, or 6-pipe heat recovery. This new feature intends to build the basic components required to operate these advanced plant configurations.

![divertingvalveplant](DivertingValvePlant.png)
<p style="text-align: center;"> Figure 1. Advanced Heat Recovery Plant Design</p>

## Approach

This new feature will include a new object **PlantEquipmentOperation:ChillerHeaterChangeover** and also include updates to the 2-pipe and 4-pipe version of a chiller heater using the existing objects *HeatPump:PlantLoop:EIR:Cooling* and *HeatPump:PlantLoop:EIR:Heating*. This new feature will not include controls for the ice storage system or auxiliary boiler discussed here. Additional equipment controls for the boiler and ice tank, if needed, may be added at a future time.

Trane has provided capacity and power performance curves for heat pump equipment that would be (and currently are) used in this type of plant. The power curve was converted to energy input ratio to be consistent with existing models in EnergyPlus. Trane also provide a new defrost model as an alternative to those used for DX heating coils. All 3 defrost models will be included in the modifications to the HeatPump:PlantLoop:EIR:Heating object.

Trane has also identified minimum and maximum operating temperature limits which are included to more accurately model this product line.

The advanced operation of the chilled and hot water plants will be managed by the supervisory control manager.


## Testing/Validation/Data Source(s)

This new feature/enhancement will be tested and demonstrated with a new test file that  includes the supervisory control manager using the new PlantEquipmentOperations object and expanded versions of the existing chiller heater objects.

## IDD Object changes

Updates to the HeatPump:PlantLoop:EIR:Cooling object are shown below. New fields are added at the end of the object such that existing example files still execute. Min-fields has not changed. Note that default values are used for each real type input filed since these inputs are past min-fields (more on that later). Existing fields in this object are not shown here:

    HeatPump:PlantLoop:EIR:Cooling,
      // the following fields are past min-fields
      A11, \field Control Type
           \note Heat pump can be controlled on leaving water temperature set point or plant load
           \type choice
           \key Setpoint
           \key Load
           \default Load
      A12, \field Flow Mode
           \note Select operating mode for fluid flow through the chiller. "ConstantFlow" is for
           \note constant pumping with flow controlled by chiller to operate at full design
           \note flow rate. "VariableSpeedPumping" is for variable pumping with flow proportional
           \note to chiller operating part load ratio.
           \type choice
           \key ConstantFlow
           \key VariableSpeedPumping
           \default ConstantFlow
      N6,  \field Minimum Part Load Ratio
           \note Below this operating limit compressor cycling will occur
           \type real
           \minimum 0.0
           \default 0.0
      N7,  \field Minimum Source Inlet Temperature
           \type real
           \units C
           \default -100.0
           \note Enter the minimum inlet outdoor air dry-bulb temperature
           \note for air-cooled units or minimum inlet water temperature for water-cooled units.
           \note The unit is disabled below this temperature.
      N8,  \field Maximum Source Inlet Temperature
           \type real
           \units C
           \default 100.0
           \note Enter the maximum inlet outdoor air dry-bulb temperature
           \note for air-cooled units or maximum inlet water temperature for water-cooled units.
           \note The unit is disabled above this temperature.
      N9,  \field Minimum Supply Water Temperature Curve Name<br>
           \type object-list<br>
           \object-list UniVariateFunctions<br>
           \note quadratic curve = a + b*OAT is typical, other univariate curves may be used<br>
           \note OAT = Outdoor Dry-Bulb Temperature<br>
      N10; \field Maximum Supply Water Temperature Curve Name<br>
           \type object-list<br>
           \object-list UniVariateFunctions<br>
           \note quadratic curve = a + b*OAT is typical, other univariate curves may be used<br>
           \note OAT = Outdoor Dry-Bulb Temperature<br>

Updates to the HeatPump:PlantLoop:EIR:Heating object are shown below. New fields are added at the end of the object such that existing example files still execute. Min-fields has not changed.  Note that default values are used for each real type input field since these inputs are past min-fields. Existing fields in this object are not shown here:

    HeatPump:PlantLoop:EIR:Heating,
      // the following fields are past min-fields
      N6,  \field Heating To Cooling Capacity Sizing Ratio
           \note Multiplies the autosized heating capacity
           \type real
           \minimum 0.0
           \default 1.0
      A11, \field Heat Pump Sizing Method
           \note Specifies sizing method when companion coil exists
           \type choice
           \key CoolingCapacity
           \key HeatingCapacity
           \key GreaterOfHeatingOrCooling
           \default CoolingCapacity
      A12, \field Control Type
           \note Heat pump can be controlled on leaving water temperature set point or plant load
           \type choice
           \key Setpoint
           \key Load
           \default Load
      A13, \field Flow Mode
           \note Select operating mode for fluid flow through the chiller. "ConstantFlow" is for
           \note constant pumping with flow controlled by chiller to operate at full design
           \note flow rate. "VariableSpeedPumping" is for variable pumping with flow proportional
           \note to chiller operating part load ratio.
           \type choice
           \key ConstantFlow
           \key VariableSpeedPumping
           \default ConstantFlow
      N7,  \field Minimum Part Load Ratio
           \note Below this operating limit compressor cycling will occur
           \type real
           \minimum 0.0
           \default 0.0
      N8,  \field Minimum Source Inlet Temperature
           \type real
           \units C
           \default -100.0
           \note Enter the minimum inlet outdoor air dry-bulb temperature
           \note for air-cooled units or minimum inlet water temperature for water-cooled units.
           \note The unit is disabled below this temperature.
      N9,  \field Maximum Source Inlet Temperature
           \type real
           \units C
           \default 100.0
           \note Enter the maximum inlet outdoor air dry-bulb temperature
           \note for air-cooled units or maximum inlet water temperature for water-cooled units.
           \note The unit is disabled above this temperature.
      A14, \field Minimum Supply Water Temperature Curve Name
           \type object-list
           \object-list UniVariateFunctions
           \note quadratic curve = a + b*OAT is typical, other univariate curves may be used
           \note OAT = Outdoor Dry-Bulb Temperature
      A15, \field Maximum Supply Water Temperature Curve Name
           \type object-list
           \object-list UniVariateFunctions
           \note quadratic curve = a + b*OAT is typical, other univariate curves may be used
           \note OAT = Outdoor Dry-Bulb Temperature
      A16, \field Dry Outdoor Correction Factor Curve Name
           \type object-list
           \object-list UniVariateFunctions
      N10, \field Maximum Outdoor Dry Bulb Temperature For Defrost Operation
           \type real
           \default -100.0
           \note defrost operation will not be active above this outdoor temperature
      A17, \field Heat Pump Defrost Control
           \type choice
           \key Timed
           \key OnDemand
           \key TimedEmpirical
           \default Timed
      N11, \field Heat Pump Defrost Time Period Fraction
           \type real
           \minimum 0.0
           \default 0.058333
           \note Fraction of time in defrost mode, default = 5 minutes.
           \note Only applicable if Timed or TimedEmpirical heat pump defrost control is specified
      A18, \field Defrost Energy Input Ratio Function of Temperature Curve Name
           \type object-list
           \object-list BivariateFunctions
           \note Biquadratic curve = a + b*WB + c*WB**2 + d*OAT + e*OAT**2 + f*WB*OAT
           \note WB = wet-bulb temperature (C) of air entering the indoor coil
           \note OAT = outdoor air dry-bulb temperature (C)
           \note Only required if Timed or OnDemand defrost strategy is specified
      A19, \field Timed Empirical Defrost Frequency Curve Name
           \type object-list
           \object-list UniVariateFunctions
           \note Quadratic curve = a + b*OAT is typical, other univariate curves may be used
           \note OAT = outdoor air dry-bulb temperature (C)
           \note Timed Empirical Defrost Frequency fraction in hours = curve output
           \note Only applicable if TimedEmpirical defrost control is specified
      A20, \field Timed Empirical Defrost Heat Load Penalty Curve Name
           \type object-list
           \object-list UniVariateFunctions
           \object-list BivariateFunctions
           \note Quadratic curve = a + b*OAT is typical, other univariate curves may be used
           \note Biquadratic curve = a + b*WB + c*WB**2 + d*OAT + e*OAT**2 + f*WB*OAT
           \note OAT = outdoor air dry-bulb temperature (C)
           \note WB = wet-bulb temperature (C) of air entering the indoor coil
           \note Timed Empirical Defrost Heat Load Penalty in watts = hot load * curve output
           \note Only applicable if TimedEmpirical defrost control is specified
      A21; \field Timed Empirical Defrost Heat Input Energy Fraction Curve Name
           \type object-list
           \object-list UniVariateFunctions
           \object-list BivariateFunctions
           \note Quadratic curve = a + b*oat is typical, other univariate curves may be used
           \note Biquadratic curve = a + b*WB + c*WB**2 + d*OAT + e*OAT**2 + f*WB*OAT
           \note OAT = outdoor air dry-bulb temperature (C)
           \note WB = wet-bulb temperature (C) of air entering the indoor coil
           \note Timed Empirical Defrost Heat Input Energy in watts = rated hot load * curve output
           \note Only applicable if TimedEmpirical defrost control is specified

The new object used as a supervisory controller is shown here:

    PlantEquipmentOperation:ChillerHeaterChangeover,
       \memo Plant equipment operation object to control switchover between chiller
       \memo and heater operation of chiller heater heat pump serving 2 plant loops. 
       \memo Poll zone loads and determine if plant should be in heating, cooling
       \memo or simultaneous heating and cooling and dispatch equipment accordingly.
    A1 , \field Name
         \required-field
         \reference ControlSchemeList
    N1 , \field Chilled Water Temperature Setpoint
         \required-field
         \type real
         \units C
    N2 , \field Hot Water Temperature Setpoint
         \required-field
         \type real
         \units C
    N3 , \field Hot Water Setpoint Reset Start Temperature
         \required-field
         \type real
         \units C
         \minimum -15.0
         \maximum 35.0
    N4 , \field Hot Water Setpoint Reset Maximum Temperature Difference
         \required-field
         \type real
         \units deltaC
         \minimum 0.0
         \maximum 11.1
         \note A blank input will result in a Hot Water Setpoint Reset Maximum Temperature Difference = 0
    N5 , \field Hot Water Setpoint Reset Ratio
         \type real
         \units percent
         \minimum -80
         \maximum 80
         \note A blank input will result in a How Water Setpoint Reset Ratio = 0
    A2 , \field Zone Load Polling ZoneList Name
         \type object-list
         \object-list ZoneListNames
    A3 , \field Cooling Only Load Plant Equipment Operation Cooling Load Name
         \type object-list
         \object-list ControlSchemeList
    A4 , \field Heating Only Load Plant Equipment Operation Heating Load Name
         \type object-list
         \object-list ControlSchemeList
    A5 , \field Simultaneous Cooling And Heating Plant Equipment Operation Cooling Load Name
         \type object-list
         \object-list ControlSchemeList
    A6 , \field Simultaneous Cooling And Heating Plant Equipment Operation Heating Load Name
         \type object-list
         \object-list ControlSchemeList
    A7 , \field Dedicated Chilled Water Return Recovery HeatPump Name
         \type object-list
         \object-list validPlantEquipmentNames
         \note Enter name of HeatPump:PlantLoop:EIR:Cooling object to control chilled water return adding heat to hot water return
    A8 ; \field Dedicated Hot Water Return Recovery HeatPump Name
         \type object-list
         \object-list validPlantEquipmentNames
         \note Enter name of HeatPump:PlantLoop:EIR:Heating object to control hot water return cooling the chilled water return
  

## Proposed additions to Meters:

HeatPump:PlantLoop:EIR:Heating

    Heat Pump Defrost Electricity Energy

## Proposed Report Variables:

HeatPump:PlantLoop:EIR:Cooling<br>
HeatPump:PlantLoop:EIR:Heating

    Heat Pump Part Load Ratio 
    Heat Pump Cycling Ratio

HeatPump:PlantLoop:EIR:Heating

    Heat Pump Load Due To Defrost
    Heat Pump Fractional Defrost Time
    Heat Pump Defrost Electricity Rate
    Heat Pump Defrost Electricity Energy

## Engineering and Input Output Reference Documents

New documentation describing each of these changes.

## Transition

Not needed.

## References

 - [Trane Ascend Air-To-Water Heat Pump](https://www.trane.com/commercial/north-america/us/en/products-systems/chillers/air-cooled-chillers/ascend-air-to-water-heat-pump.html)<br>
 - [Trane Installation, Operation, and Maintenance Ascend™ Air-Cooled Chiller
Models ACS and ACX](https://www.trane.com/content/dam/Trane/Commercial/global/products-systems/equipment/chillers/air-cooled/ascend/AC-SVX002E-EN_10062021.pdf)

## Code Design Documentation:

The HeatPump:PlantLoop:EIR object changes are fairly straight-forward. New class variables are added to represent each new input field. A code example for the new Control Type input field with a \default value = Load. Note here that the default value is not checked and assumed to be Load. The default field value will be further discussed in the next example.

    constexpr std::array<std::string_view, 
            static_cast<int>(ControlType::Num)> PLHPCtrlTypeNamesUC = {"SETPOINT", "LOAD"};
    auto const controlType = fields.find("control_type");
    if (controlType != fields.end()) {
        thisPLHP.sysControlType = static_cast<ControlType>(
                    getEnumerationValue(PLHPCtrlTypeNamesUC,
                    UtilityRoutines::MakeUPPERCase(controlType.value().get<std::string>())));
    } else { // default to Load as specified in idd, without checking for default?
        thisPLHP.sysControlType = ControlType::Load;
    }

As another example of getInput where an input field has a default specified, the Minimum Part Load Ratio field has a default of 0. The class variable representing this field will also be initialized to 0. If the default value was removed from this idd input field, the entire else section of this getInput could be removed and the result of a blank field would still be 0 without the need for the extra checking. This of course does not allow the user to change the default value in the json idd, but the user can always enter a value. Using this same methodology, the default value could be removed from the Control Type field example above. Documentation could describe that blank fields default to Load and 0, respectively.

The suggestion here is to remove defaults for specific inputs without a required-field tag and instead use the method described above and document the default action (e.g., the IORef says the field above defaults to Load if left blank). If the user wants a different result, the field can be filled in with that intent. Applies to non-required key choices and numeric inputs.

    auto const minPLR = fields.find("minimum_part_load_ratio");
    if (minPLR != fields.end()) {
        thisPLHP.minimumPLR = minPLR.value().get<Real64>();
    } else { // get default value
        Real64 defaultVal = 0.0;
        bool defaultFound = state.dataInputProcessing->inputProcessor->getDefaultValue(
            state, cCurrentModuleObject, "minimum_part_load_ratio", defaultVal);
        if (!defaultFound) {
            // excluding from coverage
            ShowWarningError(state, // LCOV_EXCL_LINE
                 format("EIR PLHP \"{}\": Heat Pump Minimum Part Load Ratio not entered and default value not found.",
                                    thisPLHP.name)); // LCOV_EXCL_LINE
                 errorsFound = true;                 // LCOV_EXCL_LINE
        } else {
            thisPLHP.minimumPLR = defaultVal;
        }
    }

The larger effort of this new feature is to develop a plant supervisory controller that determines building loads and dispatches equipment according to the user specified plant equipment operation schemes.

