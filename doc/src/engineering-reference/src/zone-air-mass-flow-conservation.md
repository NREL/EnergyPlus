# Zone Air Mass Flow Conservation

The zone air mass flow conservation object is intended to trigger zone air mass flow balance calculations.  This feature is available for zones defined in an air loop only and requires that zone mixing objects is defined.  If there are no zone mixing flows to adjacent zones, then the zone air mass flow is balanced by setting the Zone Mixing objects mass flow rate to zero. If there are no zone exhaust fans defined and there are no zone mixing objects specified, then a zone in an air loop is always balanced.  The zone simple flow objects (ventilation, ZoneCrossMixing etc.) with exception of zone mixing and infiltration are assumed to be self-balanced hence are not included in the zone air mass flow conservation calculation. Infiltration objects air mass flow is included in the zone air mass flow balance of zones that serve only as a source zone of zone mixing objects. And the infiltration mass flow rate used may be adjusted from the value calculated based on user inputs for zone air mass flow balance purpose. The zone air mass flow conservation equation includes: supply air flow rates, return air flow rates, zone exhaust fan flow rates, zone mixing objects flow rates and infiltration object flow rates. However, infiltration objects air mass flow rate are used to balance source zones only. Infiltration air mass flow is included in the zone air mass flow conservation only when the source zone supply air flow rate is not large enough to offset the source mass flow rate. A particular zone can be a source zone, receiving zone, or both depending on the number of ZoneMixing objects specified for that zone. Zone air mass flow balance calculation is performed when user specifies an optional object "ZoneAirMassFlowBalance" and set the input field "**Adjust Zone Mixing For Zone Air Mass Flow Balance**" to "Yes" and zones have at least one ZoneMixing object defined.  A zone may have multiple zone mixing objects connecting adjacent zones.  Zone air mass flow balance calculation is two steps procedure.  The modified return air mass flow rate calculation is given by:

![](media/image128.png)\


where,

![](media/image129.png) =zone return air mass flow rate, (kg/s)

![](media/image130.png) =zone exhaust air mass flow rate from exhaust fans, (kg/s)

![](media/image131.png) =zone mixing mass flow rate as a receiving zone, (kg/s)

![](media/image132.png) =zone mixing mass flow rate as a source zone, (kg/s)

![](media/image133.png) =zone supply air mass flow rate, (kg/s)

Figure 8 illustrates the zone mass flow components for an air loop system providing conditioned air to the two zones connected with a zone mixing object. Since Zone 1 is a source zone only, infiltration object is defined for zone 1 only.  The zone mixing object air flow rate depends on the user specified values and the zone air mass flow balance requirements.  When required the zone mixing object flow rate is adjusted from the user specified value for balancing purpose.

![Illustration of zone air mass flow balance](media/illustration-of-zone-air-mass-flow-balance.png)

Individual zone may be a source zone for multiple receiving zones, and at the same time the same source zone may receive mixing flows from multiple adjacent zones in an air loop.  The source and receiving mass flow rates of ZoneMixing objects are calculated from user defined mixing flow rates at the first HVAC iteration for each time step and adjusted in subsequent iterations to balance the zone air mass flow. The source zone mixing mass flow rate is calculated by tracking the mass flow rates of ZoneMixing objects connected to a zone and is given by:

![](media/image135.png)\


Determine the zone return air mass flow rate as follows:

![](media/image128.png)\


Adjust the zone mixing mass flow rates based on the current zone return air mass flow rate as follows:

![](media/image136.png)\


Then this updated receiving zone mixing air mass flow rate is distributed to the mixing objects connected to the current zone proportional to user specified design flow rate. A single zone may be connected to more than one adjacent zone using multiple ZoneMixing objects.  Thus, the mixing flow rate of each contributing mixing objects defined in the current zone is updated as follows:

![](media/image137.png)\


Zones serving as a source zone only are allowed to draw outside air using infiltration object.   Outdoor air is drawn via infiltration objects to replenish the the zone and when the zone's return air flow rates is zero. The infiltration air mass flow rate is determined as follows:

![](media/image138.png)\


![](media/image139.png)\


This infiltration air mass flow rate calculated is either added to the base infiltration air flow, which is calculated from user inputs, or overrides the base infiltration air flow depending on user choice. The two user Key choice inputs for Source Zone Infiltration Treatment are "**AddInfiltrationFlow**" and "**AdjustInfiltrationFlow**".

There is an additional constraint to the return air mass flow rate calculation.  The sum of the return air mass flow rates of zones in air loop must satisfy the air loop return air mass flow balance.  The above four sets of equations are iterated for each zone in an air loop until the convergence criterion is satisfied or until the maximum iteration limit is exceeded.

The mass conservation calculations are performed in routine CalcZoneMassBalance in ZoneEquipmentManager module. The latest ZoneMixing object flow rate is communicated back to the zone air heat balance terms. This is done by re-simulating the simple flow objects as zone equipment during each HVAC iteration.  This requires calling the routine "CalcAirFlowSimple" in "SimZoneEquipment" routine as a zone equipment.

*Zone Mass Conservation Convergence*

The zone mass conservation calculation convergence criterion is based on the absolute difference of the zone mixing objects mass flow rates between successive iteration.  If the difference is above tolerance limits of 0.00001 then the HVAC Air loop is simulated again.
