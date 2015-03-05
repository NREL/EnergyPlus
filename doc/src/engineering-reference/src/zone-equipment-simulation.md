# Zone Equipment Simulation

When the EnergyPlus HVAC simulation manager needs to simulate the zone equipment side of the air loop it calls *ManageZoneEquipment*, the zone equipment simulation manager subroutine. Like the other managers, *ManageZoneEquipment* has a very simple structure:

~~~~~~~~~~~~~~~~~~~~

            IF (GetInputFlag) THEN
              CALL GetZoneEquipment
              GetInputFlag = .FALSE.
            END IF

            CALL InitZoneEquipment(FirstHVACIteration)

            IF (ZoneSizingCalc) THEN
              CALL SizeZoneEquipment
            ELSE
              CALL SimZoneEquipment(FirstHVACIteration, SimAir)
            END IF

            CALL RecordZoneEquipment(SimAir)

            CALL ReportZoneEquipment

            SimZone = .False.

            RETURN
~~~~~~~~~~~~~~~~~~~~

#. If the user input data has not been input, get the data and store it in the zone equipment data structures
#. Perform zone equipment initialization calculations.
#. If calculation of the design zone air flow rates and loads needs to be done, do it. The results of this calculation are stored in the zone sizing data structures and used by the component automatic sizing algorithms and the central system sizing calculations.
#. Otherwise simulate all of the zone equipment.
#. Transfer the zone equipment outlet node data to the inlet nodes of the primary air systems and check for convergence (done in RecordZoneEquipment by calling UpdateHVACInterface).

## Input data

The input specifying a set of zone equipment consists of:

    #. the ZoneHVAC:EquipmentConnections object data;
    #. the zone connection to the air loop – air inlet nodes, exhaust nodes, outlet node, zone node;
    #. the components serving each zone – air terminal units, fan coils etc.;
    #. zone supply air path data;
    #. zone splitters and supply plenums;
    #. zone return air path data;
    #. zone mixer and return plenums;

## Initialization Calculations

### One Time Calculations

There are no one time calculations for zone equipment

### Begin Environment Initializations

For each controlled zone initialize the zone inlet, exhaust and zone nodes to standard conditions. Namely:

![](media/image1876.png)\


![](media/image1877.png)\


![](media/image1878.png)\


![](media/image1879.png)\


![](media/image1880.png)\


![](media/image1881.png)\


where *W~oa~* is the humidity of the outside air; ![](media/image1871.png) is the EnergyPlus psychrometric function for enthalpy *h*, given temperature and humidity ratio; *p~oa~* is the pressure of the outside air; and *Qu* is quality.

### System Time Step Initializations

For each controlled zone, set the exhaust nodes conditions equal to the zone node conditions; except set the exhaust nodes mass flow rate and min and max available mass flow rates to zero.

## Simulation

The subroutine *SimZoneEquipment* does the actual simulation of all the zone equipment. Note that this includes components that are part of the demand side of an air loop as well as components that are independent of any air loop.

For zone equipment components that are part of the demand side of an air loop the simulation sequence is effectively performed in the direction of the air flow. First the supply air plenums and zone splitters are simulated in their input order. Then the air terminal units are simulated followed by the zone return plenums and zone mixer. Each air terminal unit sets its inlet node to the air mass flow rate required to satisfy its zone load. These mass flow rates are then passed back upstream to the air loop demand-side inlet nodes at the end of each zone equipment simulation sequence. These demand-side inlet mass flow rates are then used as mass flow rate setpoints in the air loop supply-side simulation.

If multiple air-conditioning components are attached to a zone, the components are simulated in the order specified by the user assigned priority given in the ZoneHVAC:EquipmentList object.

For each full air loop there should be 1 supply air path for each primary air system outlet (i.e. 1 for single duct systems,  2 for dual duct systems). For each full air loop there should be one return air path. The supply air paths consist of any combination of zone splitters and zone supply air plenums as long as it forms a tree structure with the supply air path inlet node the root and the air terminal unit inlet nodes as the leaves. The return air path configuration is limited to a single mixer; there may be multiple return plenums.

Loop over all the supply air paths.

Loop over each component (supply plenum or zone splitter) on the supply air path and simulate each component. The components are simulated in input order.

Loop over all the controlled zones.

Set the required system output.

Loop over the components serving the zone in the user prioritized order.

Simulate each component.

Increment the required system output.

Loop over all the supply air paths

Loop over the components on each supply air path in reverse input order. This reverse order simulation passes the air terminal units inlet mass flows back upstream to the return air path inlet node.

Check to see if the supply air path inlet node mass flow rate has changed. If it has set the *SimAir* flag to *true*. This signals the HVAC manager that the supply-side of the air loop needs to be resimulated.

Calculate the zone air flow mass balance – the zone inlet and exhaust mass flow rates are summed and the zone node and return air node mass flow rates are determined by a mass balance for each zone.

Calculate the conditions at each zone return air node. Here energy not included in the zone energy balance such as light-heat-to-return-air is added to the return nodes of the controlled zones.

Loop over all of the return air paths.

Loop over each component (return plenum or zone mixer) on the return air path and simulate each component.

This completes a single simulation sequence of all the zone equipment.