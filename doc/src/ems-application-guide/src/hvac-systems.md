# HVAC Systems

## AirTerminal:SingleDuct:Uncontrolled

An internal variable called "AirTerminal:SingleDuct:Uncontrolled Maximum Mass Flow Rate" provides information about the design flow rate for direct air terminals. The units are kg/s. This is the mass flow rate (for dry air at standard conditions) at the volume flow rate entered in the field Maximum Air Flow Rate (m^3^/s) in the AirTerminal:SingleDuct:Uncontrolled input object. This internal variable is useful for scaling the flow rates assigned to the "Mass Flow Rate" control in the "AirTerminal:SingleDuct:Uncontrolled" EMS actuator.

## Fan Nominal Ratings 

### Fan Maximum Mass Flow Rate

The input variable "Fan Maximum Mass Flow Rate" provides information about the maximum flow rate for a fan. The units are kg/s. This is the mass flow rate (for dry air at standard conditions) associated with the volume flow rate entered into the Maximum Air Flow Rate (m^3^/s) in the various fan input objects. This internal variable is useful for scaling the flow rates assigned to the "Fan Air Mass Flow Rate" control in the "Fan" EMS actuator.

### Fan Nominal Pressure Rise

The input variable "Fan Nominal Pressure Rise" provides information about the static pressure rise experienced by a fan. The units are Pascals. This is the value entered into the field called Pressure Rise in the various fan input objects. This internal variable is useful for scaling the pressures assigned to the "Fan Pressure Rise" control in the "Fan" EMS actuator.

### Fan Nominal Total Efficiency

The input variable "Fan Nominal Total Efficiency" provides information about the nominal efficiency of each fan. The value is dimensionless and expressed as a fraction. This is the value entered into the field called Fan Efficiency in the various fan input objects. This internal variable is useful for scaling the value assigned to "Fan Total Efficiency" control in the "Fan" EMS actuator.

## Unitary HVAC Nominal Ratings

### Unitary HVAC Design Heating Capacity

The internal variable called "Unitary HVAC Design Heating Capacity" provides information about the nominal heating capacity of unitary or furnace HVAC equipment. The units are Watts. The source of these data will vary depending on the type of heating coil. This internal variable is useful for scaling the value assigned to "Sensible Load Request" control in the "Unitary HVAC" EMS actuator.

### Unitary HVAC Design Cooling Capacity

The internal variable called "Unitary HVAC Design Cooling Capacity" provides information about the nominal cooling capacity of unitary or furnace HVAC equipment. The units are Watts. The source of these data will vary depending on the type of cooling coil. This internal variable is useful for scaling the value assigned to "Sensible Load Request" control in the "Unitary HVAC" EMS actuator.

## Outdoor Air Mixer Nominal Ratings

### Outdoor Air Controller Maximum Mass Flow Rate

The internal variable called "Outdoor Air Controller Maximum Mass Flow Rate" provides information about the maximum outdoor air rate for each outdoor air system. The units are kg/s. The sources of these data are inputs in the Controller:OutdoorAir input object. This internal variable is useful for scaling the value assigned to the "Air Mass Flow Rate" control in the "Outdoor Air Controller" EMS actuator.

### Outdoor Air Controller Minimum Mass Flow Rate

The internal variable called "Outdoor Air Controller Minimum Mass Flow Rate" provides information about the minimum outdoor air rate for each outdoor air system. The units are kg/s. The sources of these data are inputs in the Controller:OutdoorAir input object. This internal variable is useful for scaling the value assigned to the "Air Mass Flow Rate" control in the "Outdoor Air Controller" EMS actuator.

## Pump Nominal Ratings

The internal variable called "Pump Maximum Mass Flow Rate" provides information about the size of the pump. The units are kg/s. This is the mass flow rate associated with the volume flow rate entered into the Rated Flow Rate (m^3^/s) in the various pump input objects. This internal variable is useful for scaling the flow rates assigned to the "Pump Mass Flow Rate" control in the "Pump" EMS actuator.

## Low Temperature Radiant Hydronic

### Constant Flow Low Temp Radiant Design Water Mass Flow Rate

The internal variable called "Constant Flow Low Temp Radiant Design Water Mass Flow Rate" provides information about the design water flow rate for radiant systems defined using a ZoneHVAC:LowTemperatureRadiant:ConstantFlow input object. The units are m^3^/s. This internal variable is useful for scaling the flow rates assigned to the "Water Mass Flow Rate" control in the "Constant Flow Low Temp Radiant" EMS actuator.

### Hydronic Low Temp Radiant Design Water Mass Flow Rate for Heating

The internal variable called "Hydronic Low Temp Radiant Design Water Mass Flow Rate for Heating" provides information about the heating design water flow rate for radiant systems defined using a ZoneHVAC:LowTemperatureRadiant:VariableFlow input object. The units are m^3^/s. This internal variable is useful for scaling the flow rates assigned to the "Water Mass Flow Rate" control in the "Hydronic Low Temp Radiant" EMS actuator.

### Hydronic Low Temp Radiant Design Water Mass Flow Rate for Cooling

The internal variable called "Hydronic Low Temp Radiant Design Water Mass Flow Rate for Heating" provides information about the cooling design water flow rate for radiant systems defined using a ZoneHVAC:LowTemperatureRadiant:VariableFlow input object. The units are m^3^/s. This internal variable is useful for scaling the flow rates assigned to the "Water Mass Flow Rate" control in the "Hydronic Low Temp Radiant" EMS actuator.
