NFP – Dedicated OA to supply and inlet side of zone HVAC units

FSEC, B. Nigusse, April 26, 2016

(Based on NFP_OAtoZoneUnits.doc, dated July 27, 2011)



#Justification for Feature Update:
In its current form, EnergyPlus 8.5 allow a central dedicated outdoor air system (DOAS) to provide conditioned outdoor air directly to the supply or the inlet side of *ZoneHVAC:FourPipeFanCoil* and *ZoneHVAC:WaterToAirHeatPump* zoneHVAC equipment only. Such an arrangement is commonly used in current building design practices and should be extended to other zoneHVAC equipment in EnergyPlus. While there is a credible work-around in EnergyPlus version 8.5 that is thermodynamically similar to the supply side delivery arrangement described above, the OA from the central DOAS is delivered to the zone directly instead of being mixed with the supply air at supply node of the zoneHVAC equipment. This effort will extend such capability to ***ZoneHVAC:PackagedTerminalAirConditioner***, ***ZoneHVAC:PackagedTerminalHeatPump***, or ***ZoneHVAC:TerminalUnit:VariableRefrigerantFlow*** zoneHVAC equipment. The total airflow delivered to the zone is important to be established as it helps in sizing of the duct work and supply registers for the zone, and is also the more commonly used arrangement in practice. In addition to the total airflow, supply air and outdoor air conditions may also be set taking into account the mixing taking place upstream or downstream.   

##Conference Call Conclusions:

N/A
###Other Conference Call Topics (not in scope of current proposal):

N/A

#Overview:
The arrangements shown in Figure 1 is supported in EnergyPlus version 8.5 for 4PipeFanCoil and WSPTHP ZoneHVAC equipment only. A central DOAS provides fresh air to the zone mixed either at the inlet or outlet of a local zoneHVAC equipment that conditions the recirculation air.

![Figure 1 Air Terminal OA Mixer at inlet and supply side of each zoneHVAC equipment (figures courtesy Trane/SEI associates)](DOAStoZoneHVACEquipment.png)

To allow for this configuration, the two terminal unit mixer existing objects, *AirTerminal:SingleDuct:SupplySideMixer* and *AirTerminal:SingleDuct:InletSideMixer* are used. These mixer objects can be used with all the ZoneHVAC type terminal unit objects from the group *Zone Forced Air Units* except for *ZoneHVAC:OutdoorAirUnit*, *ZoneHVAC:Dehumidifier:DX* and *ZoneHVAC:EnergyRecoveryVentilator*. The proposed task will add such capability to the following zoneHVAC equipment:

1. **ZoneHVAC:PackagedTerminalAirConditioner**
2. **ZoneHVAC:PackagedTerminalHeatPump**
3. **ZoneHVAC:TerminalUnit:VariableRefrigerantFlow**

#Approach:
Two existing terminal unit mixer objects ***AirTerminal:SingleDuct:SupplySideMixer*** and ***AirTerminal:SingleDuct:InletSideMixer*** in *Air Distribution Equipment* group will be used. A terminal unit mixer object provides the zoneHVAC equipment object type and object name, and three node names: primary air inlet node name, secondary air inlet node name, and an air outlet node name. Each mixer object will sum the airflows and average the air properties of the two incoming streams and deliver mixed air either to the zone supply node or the inlet of the local zoneHVAC equipment. The total supply air delivered to a zone is equal to the sum of the primary air and the recirculating air. The primary air flow is balanced with the zone return air flow. This is done automatically in the subroutine *CalcZoneMassBalance*, module *ZoneEqupmentManager*. These terminal units resemble the induction type terminal units: they mix supply air with recirculated zone air and deliver the mixed air into a zone. The mixer objects will contain a reference to the upstream or downstream allowed zoneHVAC equipment. If present, the mixers will be simulated and will be able to pass the zone *load met* taking into account the outdoor air conditions to the local zoneHVAC equipment calculation routines.     

###Testing/Validation/Data Source(s):
The new feature will be compared against exiting model.

##IO Ref (draft):

###AirTerminal:SingleDuct:SupplySideMixer 
The *AirTerminal:SingleDuct:SupplySideMixer* object is used to mix conditioned outdoor air from a central dedicated outdoor air system (DOAS) with conditioned recirculation air from a local zonal HVAC unit to be supplied as a single stream to the zone at its inlet node. The mixer will sum the airflows and average the air properties of the two incoming streams. Currently, the *AirTerminal:SingleDuct:SupplySideMixer* is used with the *ZoneHVAC:FourPipeFanCoil* and *ZoneHVAC:WaterToAirHeatPump* objects. This effort will expand to other three zoneHVAC equipment. 
   
**Field: Name**

Unique name for this air terminal mixer.

**Field: ZoneHVAC Terminal Unit Object Type**

The type of zoneHVAC equipment to which this terminal unit will be connected. This is a choice field. The choices are *ZoneHVAC:FourPipeFanCoil*, *ZoneHVAC:WaterToAirHeatPump*, ***ZoneHVAC:PackagedTerminalAirConditioner***, ***ZoneHVAC:PackagedTerminalHeatPump***, or ***ZoneHVAC:TerminalUnit:VariableRefrigerantFlow***.

**Field: ZoneHVAC Terminal Unit Name**

The name of the zoneHVAC equipment to which this air terminal unit will be connected.

**Field: Terminal Unit Outlet Node Name**

The name of the air outlet node of the supply side mixer. This will be an inlet air node of the conditioned zone. 

**Field: Terminal Unit Primary Air Inlet Node Name**

The name of the primary air inlet node of the supply side mixer. This will be an outlet node of an AirLoopHVAC:ZoneSplitter, providing the connection to the DOAS system. 

**Field: Terminal Unit Secondary Air Inlet Node Name**
 
The name of the secondary air inlet node of the supply side mixer. This will be the outlet air node of the zoneHVAC equipment.   

The IDD entry for this object follows.

**AirTerminal:SingleDuct:SupplySideMixer,**

       \memo The supply side mixer air terminal unit provides a means of supplying
       \memo central system air to the air outlet of a zoneHVAC equipment such as a four pipe
       \memo fan coil unit. Normally the central air would be ventilation air from a 
       \memo dedicated outdoor air system (DOAS).

   A1, \field Name

       \required-field

   A2, \field ZoneHVAC Terminal Unit Object Type

       \required-field
       \type choice
       \key ZoneHVAC:FourPipeFanCoil
       \key ZoneHVAC:WaterToAirHeatPump
       \key ZoneHVAC:PackagedTerminalAirConditioner
       \key ZoneHVAC:PackagedTerminalHeatPump
       \key ZoneHVAC:TerminalUnit:VariableRefrigerantFlow

   A3, \field ZoneHVAC Terminal Unit Name

       \required-field
       \type object-list
       \object-list DOAToZonalUnit

   A4, \field Terminal Unit Outlet Node Name

       \required-field
       \type node

   A5, \field Terminal Unit Primary Air Inlet Node Name

       \required-field
       \type node

   A6; \field Terminal Unit Secondary Air Inlet Node Name

       \required-field
       \type node


An example is shown below.

**AirTerminal:SingleDuct:SupplySideMixer,**

    SPACE4-1 DOAS Air Terminal,         !- Name
    ZoneHVAC:PackagedTerminalAirConditioner,  !- ZoneHVAC Terminal Unit Object Type
    SPACE4-1 PTAC, 				        !- ZoneHVAC Terminal Unit Name
    SPACE4-1 Supply Inlet,			    !- Terminal Unit Outlet Node Name
    SPACE4-1 AT Mixer Primary Inlet,	!- Terminal Unit Primary Air Inlet Node Name
    SPACE4-1 Heat Pump Outlet;          !- Terminal Unit Secondary Air Inlet Node Name


###AirTerminal:SingleDuct:InletSideMixer 
The *AirTerminal:SingleDuct:InletSideMixer* object is used to mix conditioned outdoor air from a central dedicated outdoor air system (DOAS) with un-conditioned recirculation air from the zone to be supplied as a single stream to the inlet of the local zoneHVAC equipment. The mixer will sum the airflows and average the air properties of the two incoming streams. Currently, the AirTerminal:SingleDuct:InletSideMixer is used with the *ZoneHVAC:FourPipeFanCoil* and *ZoneHVAC:WaterToAirHeatPump* objects. This effort will expand to other three zoneHVAC equipment. 
   
**Field: Name**

Unique name for this air terminal mixer.

**Field: ZoneHVAC Terminal Unit Object Type**

The type of zoneHVAC equipment to which this air terminal unit will be connected. This is a choice field. The choices are *ZoneHVAC:FourPipeFanCoil*, *ZoneHVAC:WaterToAirHeatPump*, ***ZoneHVAC:PackagedTerminalAirConditioner***, ***ZoneHVAC:PackagedTerminalHeatPump***, or ***ZoneHVAC:TerminalUnit:VariableRefrigerantFlow***.

**Field: Terminal Outlet Node Name**

The name of the air outlet node of the inlet side mixer. This will be the inlet air node of the zoneHVAC equipment.

**Field: Terminal Unit Primary Air Inlet Node Name**

The name of the primary air inlet node of the inlet side mixer. This will be an outlet node of an AirLoopHVAC:ZoneSplitter object, providing the connection to the DOAS system. 
  
**Field: Terminal Unit Secondary Air Inlet Node Name**

The name of the secondary air inlet node of the inlet side mixer. This will be an exhaust node of the zone that is being conditioned.   

The IDD entry for this object follows.

**AirTerminal:SingleDuct:InletSideMixer,**

       \memo The inlet side mixer air terminal unit provides a means of supplying
       \memo central system air to the air inlet of a zoneHAC equipment such as a four
       \memo pipe fan coil. Normally the central air would be ventilation air from
       \memo a dedicated outdoor air system (DOAS).

   A1, \field Name

       \required-field

   A2, \field ZoneHVAC Terminal Unit Object Type

       \required-field
       \type choice
       \key ZoneHVAC:FourPipeFanCoil
       \key ZoneHVAC:WaterToAirHeatPump
       \key ZoneHVAC:PackagedTerminalAirConditioner
       \key ZoneHVAC:PackagedTerminalHeatPump
       \key ZoneHVAC:TerminalUnit:VariableRefrigerantFlow

   A3, \field ZoneHVAC Terminal Unit Name

       \required-field
       \type object-list
       \object-list DOAToZonalUnit

   A4, \field Terminal Unit Outlet Node Name

       \required-field
       \type node

   A5, \field Terminal Unit Primary Air Inlet Node Name

       \required-field
       \type node

   A6; \field Terminal Unit Secondary Air Inlet Node Name

       \required-field
       \type node

An example is shown below.

**AirTerminal:SingleDuct:InletSideMixer,**

    SPACE1-1 DOAS Air Terminal,  	    !- Name
    ZoneHVAC:PackagedTerminalHeatPump,  !- ZoneHVAC Terminal Unit Object Type
    SPACE1-1 PTHP,      		        !- ZoneHVAC Terminal Unit Name
    SPACE1-1 Heat Pump Inlet,		    !- Terminal Unit Outlet Node Name
    SPACE1-1 ATMixer Primary Inlet,     !- Terminal Unit Primary Air Inlet Node Name
    SPACE1-1 ATMixer Secondary Inlet;   !- Terminal Unit Secondary Air Inlet Node Name


Example of PTHP served with dedicated outdoor air system (DOAS). The Outdoor Air Mixer object type and name input fields are left blank and the outdoor air flow rates in the PTHP are set to zero.

 **ZoneHVAC:PackagedTerminalHeatPump,**

    SPACE1-1 Heat Pump,      !- Name
    FanAvailSched,           !- Availability Schedule Name
    SPACE1-1 Heat Pump Inlet,!- Air Inlet Node Name
    SPACE1-1 Supply Inlet,   !- Air Outlet Node Name
    ,                        !- Outdoor Air Mixer Object Type
    ,                        !- Outdoor Air Mixer Name
    Autosize,                !- Supply Air Flow Rate During Cooling Operation {m3/s}
    Autosize,                !- Supply Air Flow Rate During Heating Operation {m3/s}
    ,                        !- Supply Air Flow Rate When No Cooling or Heating is Needed {m3/s}
    0,                       !- Outdoor Air Flow Rate During Cooling Operation {m3/s}
    0,                       !- Outdoor Air Flow Rate During Heating Operation {m3/s}
    0,                       !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}
    Fan:OnOff,               !- Supply Air Fan Object Type
    SPACE1-1 Supply Fan,     !- Supply Air Fan Name
    Coil:Heating:DX:SingleSpeed,  !- Heating Coil Object Type
    SPACE1-1 HP Heating Mode,     !- Heating Coil Name
    0.001,                   !- Heating Convergence Tolerance {dimensionless}
    2.0,                     !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}
    Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type
    SPACE1-1 HP Cooling Mode,     !- Cooling Coil Name
    0.001,                   !- Cooling Convergence Tolerance {dimensionless}
    Coil:Heating:Gas,        !- Supplemental Heating Coil Object Type
    SPACE1-1 HP Supp Coil,   !- Supplemental Heating Coil Name
    50.0,                    !- Maximum Supply Air Temperature from Supplemental Heater {C}
    20.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}
    BlowThrough,             !- Fan Placement
    CyclingFanSch;           !- Supply Air Fan Operating Mode Schedule Name

###Proposed Report Variables:
N/A

###Proposed additions to Meters:
N/A

##EngRef (draft):
<required>

###Example File and Transition changes:
New example files using both inlet and supply side OA mixing configurations will be provided for each ZoneHVAC equipment added. 

###Other documents:
As needed.
