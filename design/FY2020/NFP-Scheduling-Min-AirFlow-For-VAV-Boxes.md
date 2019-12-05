<head>
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({
  extensions: ["tex2jax.js"],
  jax: ["input/TeX", "output/HTML-CSS"],
  tex2jax: {
   inlineMath: [ ['$','$'], ["\\(","\\)"] ],
   displayMath: [ ['$$','$$'], ["\\[","\\]"] ],
   processEscapes: true
  },
  "HTML-CSS": { availableFonts: ["TeX"] }
  });
</script>
  <script type="text/javascript"
   src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>
</head>

#Scheduling Minimum Airflow for VAV Boxes#
==========================================================================

**Florida Solar Energy Center**

 - *B. Nigusse*

 - Original date: December 5, 2019


## Justification for Feature Update ##
Many inpatient and outpatient healthcare facilities in the US and around the world are governed by ASHRAE Standard 170 or similar standards that require minimum supply air flow rates during occupied mode operation by space type. In order to maintain thermal comfort, this minimum air flow rate may be exceeded and in perimeter spaces the design air flow can be twice as high the design minimum air flow. But during unoccupied mode operation, the minimum air flow based on space type can be either zero, the box minimum, or the minimum required to maintain air flow direction between spaces.

Although EnergyPlus supports minimum airflow schedule, this schedule applies to the calculated peak airflow in the zone. Users are currently unable to model the appropriate minimum airflow turndowns. 

Adding a “Minimum Flow Schedule” that applies to the design minimum air flow, will allow users the flexibility to better model actual building operation.
   

###Conference Call Conclusions:
N/A

###Other Conference Call Topics (not in scope of current proposal):

N/A

## Overview ##

All the six VAV air terminal units in EnergyPlus has input for specifying a constant minimum air flow fraction. This fraction is defined relative to the air terminal maximum air flow and used to determine the *design minimum* supply air flow. However, the "AirTerminal:SingleDuct:VAV:Reheat" and "AirTerminal:SingleDuct:VAV:NoReheat" have two more method of specifying the minimum flow input methods (1) *Fixed Minimum Air Flow Rate*, and (2) *Minimum Air Flow Fraction Schedule Name*.

The *Fixed Minimum Air Flow Rate* input method allows to enter a fixed minimum flow rate, and the *Minimum Air Flow Fraction Schedule Name* input method allows to enter a scheduled minimum air flow fractions. This existing minimum flow fraction applies the peak design flow rate only. The following two VAV air terminal objects have three different input methods for specifying the design minimum air flow: 

 - **AirTerminal:SingleDuct:VAV:Reheat**
 - **AirTerminal:SingleDuct:VAV:NoReheat**

The following four VAV air terminal object types have only one input method for specifying the minimum air flow and that is *Zone Minimum Air Flow Fraction* input method and this fraction is defined relative to the air terminal maximum (peak) air flow.

 - **AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan**
 - **AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat**
 - **AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat**
 - **AirTerminal:DualDuct:VAV**

The air terminal "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan" minimum air flow is used when there is no load (or when the air terminal VAV fan is off). The use of the minimum air flow for this air terminal object is slightly different.

## Approach ##

The approach this enhancement implementation is to add a new input field that operates on the design minimum air flow. The value of this schedule multiplies the design minimum air flow. This enhancement gives users the flexibility to adjust the box design minimum air flow for the different building application needs.

###New Input Field"
"Operating Minimum Air Flow Fraction Schedule Name"



### Pseudocode ###


#### Calculating of Operating (actual) Minimum Air Flow at the Air Terminal Box ####

Currently the design minimum air flow is determined from user specified minimum air flow fraction and the maximum (peak) air flow as follows:

***DesignMinimumFlowRate*** is the design minimum air flow determined based one of the three methods currently supported.

***DesignMinimumFlowRate = Sys.AirMassFlowRateMax * Sys.ZoneMinAirFrac***

This enhancement will allow adjusting the design minimum air flow to determine operating minimum air flow using the fraction values specified in the new input field as follows:
 
**OperatingMinimumFlowRate = DesignMinimumFlowRate * OperatingMinimumFraction**

or

***OperatingMinimumFlowRate = Sys.AirMassFlowRateMax * Sys.ZoneMinAirFrac * OperatingMinimumFraction***

***OperatingMinimumFraction*** is the *Operating Minimum Air Flow Fraction*, which is obtained from the new input field and multiplies the design minimum air flow.


If there is outdoor air flow required for ventilation, then the actual minimum will be the maximum of the operating minimum air flow calculated above and the current outdoor air flow requirement as shown below:

***OperatingMinimumFlowRate = max(OperatingMinimumFlowRate, CurrentOAMassFlowRate)***


And this operating minimum air flow is checked against the minimum and maximum air flow of the air terminal boxes set during initialization at every iteration as shown below:

***OperatingMinimumFlowRate = max(OperatingMinimumFlowRate, SysInlet.AirMassFlowRateMinAvail)***

***OperatingMinimumFlowRate = min(OperatingMinimumFlowRate, SysInlet.AirMassFlowRateMaxAvail)***


The system inlet minimum available air flow rate will be the operating minimum and is calculated as follows:

***SysInlet.AirMassFlowRateMinAvail = Sys.AirMassFlowRateMax * Sys.ZoneMinAirFrac * OperatingMinimumFraction***


The maximum air flow during reheat will not be adjusted by the new field, will remain the design minimum and is given by:

***MaxAirVolFlowRateDuringReheat = Sys.MaxAirVolFlowRate * Sys.ZoneMinAirFrac***


If the new input field is blank, then the **OperatingMinimumFraction** variable will be set to 1.0.

###Testing/Validation/Data Source(s):
The new feature will be compared against exiting model. A new example file will be provided as needed.


##IO Ref (draft):

The new input field will be appended to the end of the existing air terminal VAV objects. Modified "AirTerminal:SingleDuct:VAV:Reheat" object is shown below as a sample.


AirTerminal:SingleDuct:VAV:Reheat,

       \memo Central air system terminal unit, single duct, variable volume, with reheat coil (hot
       \memo water, electric, gas, or steam).
       \min-fields 18

  A1 , \field Name

       \required-field
       \reference AirTerminalUnitNames
       \reference AFNTerminalUnitNames

  A2 , \field Availability Schedule Name

       \note Availability schedule name for this system. Schedule value > 0 means the system is available.
       \note If this field is blank, the system is always available.
       \type object-list
       \object-list ScheduleNames
 
  A3 , \field Damper Air Outlet Node Name

       \note the outlet node of the damper and the inlet node of the reheat coil
       \note this is an internal node to the terminal unit and connects the damper and reheat coil
       \required-field
       \type node

  A4 , \field Air Inlet Node Name

       \note the inlet node to the terminal unit and the damper
       \required-field
       \type node

  N1 , \field Maximum Air Flow Rate

       \required-field
       \units m3/s
       \minimum 0.0
       \autosizable

  A5 , \field Zone Minimum Air Flow Input Method

       \type choice
       \key Constant
       \key FixedFlowRate
       \key Scheduled
       \default Constant
       \note Constant = Constant Minimum Air Flow Fraction (a fraction of Maximum Air Flow Rate)
       \note FixedFlowRate = Fixed Minimum Air Flow Rate (a fixed minimum air volume flow rate)
       \note Scheduled = Scheduled Minimum Air Flow Fraction (a fraction of Maximum Air Flow

  N2 , \field Constant Minimum Air Flow Fraction

       \type real
       \autosizable
       \default autosize
       \note This field is used if the field Zone Minimum Air Flow Input Method is Constant
       \note If the field Zone Minimum Air Flow Input Method is Scheduled, then this field
       \note is optional; if a value is entered, then it is used for sizing normal-action reheat coils.
       \note If both this field and the following field are entered, the larger result is used.
       \note The values for autosizing are picked up from the Sizing:Zone input fields
       \note "Cooling Minimum Air Flow per Zone Floor Area", "Cooling Minimum Air Flow", and
       \note "Cooling Minimum Air Flow Fraction". If there is no sizing calculation a default of
       \note 0.000762 m3/s-m2 (0.15 cfm/ft2) is used.

  N3 , \field Fixed Minimum Air Flow Rate

       \type real
       \units m3/s
       \autosizable
       \default autosize
       \note This field is used if the field Zone Minimum Air Flow Input Method is FixedFlowRate.
       \note If the field Zone Minimum Air Flow Input Method is Scheduled, then this field
       \note is optional; if a value is entered, then it is used for sizing normal-action reheat coils.
       \note If both this field and the previous field are entered, the larger result is used.
       \note The values for autosizing are picked up from the Sizing:Zone input fields
       \note "Cooling Minimum Air Flow per Zone Floor Area", "Cooling Minimum Air Flow", and
       \note "Cooling Minimum Air Flow Fraction". If there is no sizing calculation a default of
       \note 0.000762 m3/s-m2 (0.15 cfm/ft2) is used.

  A6 , \field Minimum Air Flow Fraction Schedule Name

       \type object-list
       \object-list ScheduleNames
       \note This field is used if the field Zone Minimum Air Flow Input Method is Scheduled
       \note Schedule values are fractions, 0.0 to 1.0.
       \note If the field Constant Minimum Air Flow Fraction is blank, then the average of the
       \note minimum and maximum schedule values is used for sizing normal-action reheat coils.

  A7 , \field Reheat Coil Object Type

       \required-field
       \type choice
       \key Coil:Heating:Water
       \key Coil:Heating:Electric
       \key Coil:Heating:Fuel
       \key Coil:Heating:Steam

  A8 , \field Reheat Coil Name

       \required-field
       \type object-list
       \object-list HeatingCoilName

  N4 , \field Maximum Hot Water or Steam Flow Rate

       \note Not used when reheat coil type is gas or electric
       \units m3/s
       \minimum 0.0
       \autosizable
       \ip-units gal/min

  N5 , \field Minimum Hot Water or Steam Flow Rate

       \note Not used when reheat coil type is gas or electric
       \units m3/s
       \minimum 0.0
       \default 0.0
       \ip-units gal/min

  A9 , \field Air Outlet Node Name

       \required-field
       \type node
       \note The outlet node of the terminal unit and the reheat coil.
       \note This is also the zone inlet node.

  N6 , \field Convergence Tolerance

       \type real
       \minimum> 0.0
       \default 0.001

  A10, \field Damper Heating Action

       \type choice
       \key Normal
       \key Reverse
       \key ReverseWithLimits
       \default ReverseWithLimits
       \note Normal means the damper is fixed at the minimum position in heating mode
       \note Reverse means the damper can open fully during reheat
       \note ReverseWithLimits means the damper will open partially during reheat
       \note as specified in the following 2 fields

  N7 , \field Maximum Flow per Zone Floor Area During Reheat

       \type real
       \units m3/s-m2
       \autosizable
       \default autosize
       \note Used only when Reheat Coil Object Type = Coil:Heating:Water and Damper Heating Action = ReverseWithLimits
       \note When autocalculating, the maximum flow per zone is set to 0.002032 m3/s-m2 (0.4 cfm/sqft)
       \note This optional field limits the maximum flow allowed in reheat mode.
       \note At no time will the maximum flow rate calculated here exceed the value of
       \note Maximum Air Flow Rate.

  N8 , \field Maximum Flow Fraction During Reheat

       \type real
       \autosizable
       \default autosize
       \note Used only when Reheat Coil Object Type = Coil:Heating:Water and Damper Heating Action = ReverseWithLimits
       \note When autocalculating, the maximum flow fraction is set to the ratio of
       \note 0.002032 m3/s-m2 (0.4 cfm/sqft) multiplied by the zone floor area and the
       \note Maximum Air Flow Rate.
       \note This optional field limits the maximum flow allowed in reheat mode.
       \note At no time will the maximum flow rate calculated here exceed the value of
       \note Maximum Air Flow Rate.

  N9 , \field Maximum Reheat Air Temperature

       \type real
       \units C
       \minimum> 0.0
       \note Specifies the maximum allowable supply air temperature leaving the reheat coil.
       \note If left blank, there is no limit and no default. If unknown, 35C (95F) is recommended.

  A11, \field Design Specification Outdoor Air Object Name

       \type object-list
       \object-list DesignSpecificationOutdoorAirNames
       \note When the name of a DesignSpecification:OutdoorAir object is entered, the terminal
       \note unit will increase flow as needed to meet this outdoor air requirement.
       \note If Outdoor Air Flow per Person is non-zero, then the outdoor air requirement will
       \note be computed based on the current number of occupants in the zone.
       \note At no time will the supply air flow rate exceed the value for Maximum Air Flow Rate.
       \note If this field is blank, then the terminal unit will not be controlled for outdoor air flow.

  A12; \field Operating Minimum Air Flow Fraction Schedule Name

       \type object-list
       \object-list ScheduleNames
       \note This field adjusts the design minimum flow rate by multiplying it using this scheduled fraction
       \note values. This field can be used with any of the three "Zone Minimum Air Flow Input Method"   
       \note Schedule values are fractions, 0.0 to 1.0. This field is intended for use with 
       \note ASHRAE Standard 170.
       \note If this field is left blank, then the operating minimum air flow fraction value is set to 1.0


