# Primary Air System Simulation

When the EnergyPlus HVAC simulation manager needs to simulate the primary air system side of the air loop it calls *ManageAirLoops*, the primary air system simulation manager subroutine.

> Note that "air loop" is used inconsistently in the program: sometimes it means the full loop consisting of both supply & demand sides – primary air system and zone equipment; sometimes it means just the supply side – the primary air system.

Like the other manager routines in EnergyPlus, *ManageAirLoops* has a very simple structure:

Table: ManageAirLoop Code

~~~~~~~~~~~~~~~~~~~~

    IF (GetInputFlag) THEN  !First time subroutine has been entered
          CALL GetAirPathData ! Get air loop descriptions from input file
          GetInputFlag=.false.
      END IF

        ! Initialize air loop related parameters
      CALL InitAirLoops(FirstHVACIteration)

        ! Call the AirLoop Simulation
      IF (SysSizingCalc) THEN
        CALL SizeAirLoops
      ELSE
        CALL SimAirLoops(FirstHVACIteration,SimZoneEquipment)
      END IF

      ! No Update

        ! Report information at the Manage Air Loop Level
      CALL ReportAirLoops
~~~~~~~~~~~~~~~~~~~~

#. If the user input data has not been input, get the data and store it in the air loop data structures.
#. Perform air loop initialization calculations:

    #. at the beginning of the simulation (one time initializations);
    #. at the start of each environment (design day or simulation run period);
    #. before each air loop simulation.

#. If automatic sizing of the loop flow rates is called for, do it.
#. Otherwise perform a simulation of the air loop.

## Input data

The input data specifying an air loop consists of:

    #. the loop configuration;
    #. Splitters, Mixers, and Branches;
    #. Components on the Branches
    #. loop control;
    #. Controllers;
    #. System Availability Managers;
    #. connection to zone equipment;
    #. design flow rate.

These objects and their data are described in the EnergyPlus *Input Output Reference* document. The utility routines used to get and check the data are described in the EnergyPlus *Guide for Module Developers*, section Input Services.

## Initialization Calculations

### One Time Calculations

#### Zones Served by System

The main one time calculation involves figuring out what zones are served by each air loop. The EnergyPlus input does not explicitly describe which zones receive supply air from a given air loop. Instead that knowledge is embedded implicitly in the overall air loop – zone equipment network description. For sizing calculations it is important to have a data structure that explicitly shows which zones each air loop serves. For instance, the air loop design supply air flow rate is obtained by summing the design heating or cooling air flow rates of the zones connected to the air loop.

For each air loop, the following calculation is performed.

For each air loop outlet branch, the corresponding zone equipment inlet node is identified.

This node number is compared to the inlet node of all AirLoopHVAC:SupplyPaths. When a match is found, the AirLoopHVAC:ZoneSplitter for this supply path is identified.

The outlet nodes of the AirLoopHVAC:ZoneSplitter are compared to the cooling inlet nodes of all the zone ZoneHVAC:AirDistributionUnits. When a match is found this zone is identified as being served by cooling supply air from the air loop.

Similarly the outlet nodes of the AirLoopHVAC:ZoneSplitter are compared with the heating inlet nodes of all ZoneHVAC:AirDistributionUnits. A match indicates that this zone is served by heating supply air from the air loop.

The case where there is no AirLoopHVAC:ZoneSplitter for a AirLoopHVAC:SupplyPath must be handled. In this case the program looks for a match between the zone equipment inlet node and an ZoneHVAC:AirDistributionUnit heating or cooling inlet node. When a match is found that zone is identified as being served with heating or cooling supply air from the air loop.

The list of cooled and heated zones are saved in the air loop data structure AirToZoneNodeInfo.

#### Branch Sizing

If this *not* an air loop sizing calculation, but is the first pass through the HVAC code in a normal simulation, loop over all the branches in all air loops and trigger the branch design air flow auto-sizing calculation. The actual calculation is described in the Sizing section of this document.

### Begin Environment Initializations

For each air loop, loop over all the branches in the loop. Initialize each branch mass flow rate:

![](media/image1856.png)\


![](media/image1857.png)\


where ![](media/image1858.png)  is the density of air at 20^^degrees C, humidity ratio = 0, standard pressure.

For each branch, loop over all the nodes on the branch and set the node data to the following values:

![](media/image1859.png)\


![](media/image1860.png)\


![](media/image1861.png)\


![](media/image1862.png)\


![](media/image1863.png)\


![](media/image1864.png)\


![](media/image1865.png)\


![](media/image1866.png)\


![](media/image1867.png)\


![](media/image1868.png)\


![](media/image1869.png)\


where ![](media/image1870.png)  is the humidity ratio of the outside air; ![](media/image1871.png) is the EnergyPlus psychrometric function for enthalpy *h*, given temperature and humidity ratio; and *Qu* is quality.

### System Time Step Initializations

For each branch in each air loop, loop over all the nodes on the branch and set ![](media/image1872.png) ; if it is the start of an HVAC solution sequence set ![](media/image1873.png) . Then set the mass flow rate setpoints for the air loop nodes.

#. On each air loop, loop over the outlet branches and find the loop outlet nodes. If it is the start of an HVAC solution sequence, set ![](media/image1874.png) . This will insure that during the first pass through the full loop that the mass flow rate will be at the maximum. Otherwise, set ![](media/image1875.png) . This sets the air loop flow rate to the total zone requirement.
#. Pass the mass flow rate setpoint upstream to the start of the outlet branches; through the splitter, if one exists; and upstream to the beginning node of the splitter inlet branch.
#. Sum the total return air mass flow rate and save it in the AirLoopFlow data structure.
#. For each air loop, loop over the inlet nodes and, at the start of each HVAC solution sequence, set the entering node mass flow rate equal to the primary air system design mass flow rate (subject to it not being larger than the entering node mass flow rate setpoint).

## Central air system simulation

The subroutine *SimAirLoops* does the actual simulation the central air system equipment for all the air loops. The simulation of a full air loop (central equipment plus zone terminal units and equipment) requires the interaction of 2 managers: *ManageAirLoops* and *ManageZoneEquipment*. Thus a single call to *SimAirLoops* results in a simulation of all the central air system equipment, but this is one part of a larger iterative simulation of the full air loops involving the zone equipment as well.

*SimAirLoops* accomplishes its task using a set of nested loops.

- Loop over all of the central air systems (*Air Primary Loops*).

- For each air system, make 1 or 2 simulation passes

- Loop over each controller in the *Air Primary Loop*

- For each controller, repeat the simulation of all the *Air Primary Loop* components until the controller has converged

- Loop over each branch in the *Air Primary Loop*

- On each branch, simulate in sequence each component

During and at the end of each loop some tests are performed.

At the end of the first pass of loop 2, a decision is made on whether a second pass is needed. The first pass has been performed assuming that there is a mass flow balance in the central air system simulation. This is usually the case. A call to *ResolveSysFlow* checks the mass balance and imposes a mass balance if there is not a balance. The lack of a system mass balance requires a resimulation of the central air system: i.e., a second pass in loop 2.

In loop 3 a call to *ManageControllers* simulates controller action and checks for controller convergence. If convergence is achieved loop 3 is exited.

After all the controllers on a loop are converged,  steps 5 & 6 are repeated one more time to ensure all the components on the loop have final values.

At the end of the primary air system simulation a call to *UpdateHVACInterface* passes the primary air system outlet node data to the zone equipment inlet nodes. If the data across the supply side – demand side gap doesn't match to within a preset tolerance, the flag *SimZoneEquipment* is set to *true* to ensure that the zone equipment side gets resimulated. Finally a flag indicating whether the economizer is active is set. This flag is used at a higher level to decide whether the primary air system needs to be resimulated if an HVAC component is calling for economizer lockout.