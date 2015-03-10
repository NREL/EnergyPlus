# Ice Thermal Storage

## Simple Ice Storage Model

### Overview

This model (object name ThermalStorage:Ice:Simple) simulates the performance of an ice storage tank. The model is based on an integrated chiller and storage tank model developed for a special optimization project (Henze and Krarti 2002). This implementation removed the integrated chiller and allows the tank to be charged by any chiller configuration in an EnergyPlus plant loop..

This thermal storage model is based on a simple simulation of an ice storage tank with a fixed capacity.  The tank is charged, or frozen, in an ice-on-coil configuration where ice builds up on the outside of the tubes carrying the brine or glycol solution from the chiller.  There are two discharge (melt) options, internal or external.  Internal melt uses the same fluid tubes for charging and discharging.  External melt uses a separate fluid path for discharge such that the outer layers of ice melt first.  The ice storage model includes an implied 3-way valve to control the amount if charge/discharge based on the incoming water temperature and the outlet node setpoint temperature.  The tank is controlled using the PlantEquipmentOperation:ComponentSetpoint plant operation scheme, and requires that a setpoint be placed by a setpoint manager on the ice storage Plant Outlet Node.  The model currently does not calculate any tank losses.

### Model Description

#### Initialization

The storage tank is assumed to be fully charged (full of ice) at the beginning of each environment.  The tank is then allowed to charge and discharge during the warmup days of the environment.

#### Control

The ice storage module is a passive component and will accept any flow rate placed on the inlet node.  The load on the ice storage tank is determined by the inlet node flow rate, the inlet node temperature, and the outlet node setpoint temperature.  A positive load indicates a request for cooling and the tank discharges if possible.  A negative load indicates a request for charging the tank if possible.  A zero load indicates the tank is dormant in which case all flow bypasses the tank and the outlet node temperature is set to the inlet node temperature.  The model includes an implied 3-way valve which controls the leaving water temperature to the outlet node setpoint temperature if possible.

#### Charging

When charging is requested, the following limits are calculated to determine the actual charging rate:

#. If the entering water temperature is greater than –1C, the charging rate is zero.
#. If the entering water temperature is greater than or equal to the outlet setpoint temperature, the charging rate is zero.
#. If the current fraction of ice stored is 1.0, the tank is fully charge, and the charging rate is zero.
#. If the requested charging rate times the current system timestep is more than the remaining uncharged storage capacity, the charging rate is limited to that required to completely fill the tank.
#. The maximum charging rate which the chilled water flow can provide is determined by the entering water temperature and flow rate and an outlet temperature which is the minimum of –1C or the outlet setpoint temperature.
#. The maximum charging rate which the tank can accept is calculated by the following equations developed in the prior work (Henze and Krarti 2002):

*UAIceCh* = ( 1.3879 - 7.6333\**y* + 26.3423\**y*^2^ - 47.6084\**y*^3^ + 41.8498\**y*^4^ - 14.2948\**y*^5^ ) \* *ITSNomCap* / *TimeInterval* / 10.0

where:

*UAIceCh* = UA rate for charging

*y*= Current ice fraction stored

*ITSNomCap*= Nominal storage capacity [GJ]

TimeInterval= 3600 [s]

The smallest charging rate determined by the above rules is selected and the corresponding leaving water temperature is calculated.

#### Discharging

When discharging is requested, the following limits are calculated to determine the actual charging rate:

#. If the entering water temperature is less than 1C, the discharge rate is zero.
#. If the entering water temperature is less than or equal to the outlet setpoint temperature, the discharge rate is zero.
#. If the current fraction of ice stored is 0.0, the tank is fully discharged, and the discharge rate is zero.
#. If the requested discharge rate times the current system timestep is more than the remaining charged storage capacity, the discharge rate is limited to that required to completely deplete the ice in the tank.
#. The maximum discharge rate which the chilled water flow can accept is determined by the entering water temperature and flow rate and an outlet temperature which is the maximum of 1C or the outlet setpoint temperature.
#. The maximum discharge rate which the tank can deliver is calculated by the following equations developed in the prior work (Henze and Krarti 2002):

For ice-on-coil internal melt:

*UAIceDisCh* = ( 1.3879 - 7.6333\**y* + 26.3423\**y*^2^ - 47.6084\**y*^3^ + 41.8498\**y*^4^ - 14.2948\**y*^5^ ) \* *ITSNomCap* / *TimeInterval* / 10.0

For ice-on-coil external melt:

*UAIceDisCh* = ( 1.1756 - 5.3689\**y* + 17.3602\**y*^2^ - 30.1077\**y*^3^ + 25.6387\**y*^4^ - 8.5102\**y*^5^ ) \* *ITSNomCap* / *TimeInterval* / 10.0

where:

UAIceDisCh= UA rate for discharging

*y*= 1 - Current ice fraction stored

*ITSNomCap*= Nominal storage capacity [GJ]

TimeInterval= 3600 [s]

The smallest discharge rate determined by the above rules is selected and the corresponding leaving water temperature is calculated.

### References

Henze, Gregor P. and Moncef Krarti. 2002. Predictive Optimal Control of Active and Passive Building Thermal Storage Inventory, Final Report for Phase I:  Analysis, Modeling, and Simulation.  U.S. Department of Energy National Energy Technology Laboratory Cooperative Agreement DE-FC-26-01NT41255, December 2002.

## Detailed Ice Storage Model

The following section describes how the detailed ice storage model works in EnergyPlus (object name ThermalStorage:Ice:Detailed).

### Charging and Discharging Equation

The actual performance of the ice storage unit depends on the physical geometry, materials, and characteristics of the ice storage unit.  In analyzing performance data trends from ice storage manufacturers, it was determined that the following equation would work well for the discharging process:

![](media/image3123.png)\


or

![](media/image3124.png)\


where:

![](media/image3125.png)\


![](media/image3126.png)\


![](media/image3127.png)\


q is the instantaneous heat transfer rate,

Qstor is the total latent storage capacity,

t is a time step used in the curve fit (usually one hour),

T~nominal~ is a nominal temperature difference (18°F = 10°C),

T~brine,in~ is the tank brine inlet temperature,

T~brine,out~ is the tank brine outlet temperature,

T~freeze~ is the freezing temperature of water or the latent energy storage material,

P~c~ is the fraction charged, and

P~d~ is the fraction discharged.

Likewise, the charging process of the ice storage device can be characterized by the following equation that is similar in form to the discharging equation:

![](media/image3128.png)\


Note that the time step might differ from the time step used within the EnergyPlus simulation.  These are actually two separate time steps and are kept separate.

### Charging Algorithm

During charging, manufacturers have stated that they attempt to charge the unit at the maximum rate until the unit is completely charged.  This, of course, occurs during off-peak electric hours.  Thus, once the setpoint has been scheduled for charging, the unit will charge at the maximum possible rate.  This means that the flow rate through the ice storage device equals the flow to the component (or no bypass).  The only time flow to the ice storage unit would be reduced is at the end of the charge cycle when more ice making capacity is available in a particular time step than is needed to fully charge the tank.  In this case, the flow to the tank would be reduced appropriately to top off the tank storage capacity.  We also have a setpoint goal for the outlet temperature of the ice storage device as defined by the setpoint schedule.

In solving the performance of the ice storage unit, we have effectively two equations (one of which is non-linear) and two unknowns.  The equations are:

![](media/image3129.png)\


from which we can obtain the component load (q) and

![](media/image3130.png)\


Both of these equations have q and T~o~ as unknowns.  However, since the setpoint temperature is the goal for T~o~, we can use this as an initial guess for T~o~.  Below is an outline of the algorithm:

- Initialize T~o~= T~set~
- Calculate LMTD\*
- Calculate q\* from charging equation for the current percent charged (We will assume that the EnergyPlus time step is sufficiently small so that we do not need to find the average percent charged for the time step.  This was necessary when one hour time steps were used as in BLAST, but EnergyPlus generally uses relatively short time steps.  Since there is already some iteration involved in the solution, we would like to avoid another layer of iteration if at all possible.  One alternative that could be implemented would be to make a second pass with a closer average value based on what happens during the time step.  This would effectively double the execution time for the model and would need to be justified before implementation.)
- Calculate T~o~~,new~ and compare it to T~o~
- Use T~o~~,new~ to calculate a new LMTD\* and iterate until T~o~ converges to some acceptable level

Charging would continue in subsequent time steps until the final state of the ice storage unit at the end of a particular time step is fully charged.  If running a chiller would "overcharge" the tank, then the flow to the tank would be reduced (greater than zero bypass flow) while maintaining the same setpoint temperature coming out of the tank (though not necessarily out of the component).

### Discharging Algorithm

During discharging, we cannot assume that all of the flow is sent through the ice storage unit and thus some of it may be bypassed around it locally.  This ice storage model includes a built-in bypass leg to accommodate this without requiring the user to enter this additional information.  This also allows the bypass leg/valve to be controlled by the ice storage unit.

While we cannot assume that all of the flow is sent through the ice storage unit, we can use that as an initial guess in order to determine the current performance of the ice storage system.  Most of the discharging algorithm then becomes very similar to the charging process.

In solving the performance of the ice storage unit, we have effectively two equations (one of which is non-linear) and two unknowns.  The equations are:

![](media/image3131.png)\


from which we can obtain the component load (q) and

![](media/image3132.png)\


Both of these equations have q and T~o~ as unknowns.  However, since the setpoint temperature is the goal for T~o~, we can use this as an initial guess for T~o~.  Below is an outline of the algorithm:

- Initialize T~o~= T~set~
- Calculate LMTD\*
- Calculate q\* from charging equation for the current percent charged (we will assume that the EnergyPlus time step is sufficiently small so that we do not need to find the average percent charged for the time step; this was necessary when one hour time steps were used as in BLAST, but EnergyPlus generally uses relatively short time steps)
- Calculate T~o~~,new~ and compare it to T~o~
- Use T~o~~,new~ to calculate a new LMTD\* and iterate until T~o~ converges to some acceptable level
- Once T~o~ has converged, we need to compare this value again to T~set~.  If T~o~ is greater than or equal to T~set~, then we either just met the load (equal) or can't quite meet it (greater than).  In this case, we simply leave T~o~ as is because we cannot meet the setpoint temperature.
- If T~o~ is less than T~set~, then we have more capacity available than we need.  In this case, we need to bypass some of the flow.  Since the load on the ice storage device and the outlet temperature are not changing (we are just reducing the flow), we only need to split the flow and do not need to recalculate the action of the ice storage device.  Some systems may be slightly dependent on the actual flow through the device itself.  However, in an actual application, this only means that a slightly different amount will bypass the device.  The overall energy impact will be the same and thus it is not necessary to be concerned about flow rate dependence.

Discharging would continue in subsequent time steps until the final state of the ice storage unit at the end of a particular time step is fully discharged.

### References

Strand, R.K. 1992. "Indirect Ice Storage System Simulation," M.S. Thesis, Department of Mechanical and Industrial Engineering, University of Illinois at Urbana-Champaign.