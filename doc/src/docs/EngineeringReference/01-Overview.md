![](media/ep.gif)

<br/>
<p><h1>EnergyPlus<sup>TM</sup> Documentation</h1></p>
<hr>
<h1>Engineering Reference</h1>
<h2>The Reference to EnergyPlus Calculations</h2>
<br/>
<p><i>(in case you want or need to know)</i></p>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<p><small>COPYRIGHT (c) 1996-2015 THE BOARD OF TRUSTEES OF THE UNIVERSITY OF ILLINOIS AND THE REGENTS OF THE UNIVERSITY OF CALIFORNIA THROUGH THE ERNEST ORLANDO LAWRENCE BERKELEY NATIONAL LABORATORY. ALL RIGHTS RESERVED. NO PART OF THIS MATERIAL MAY BE REPRODUCED OR TRANSMITTED IN ANY FORM OR BY ANY MEANS WITHOUT THE PRIOR WRITTEN PERMISSION OF THE UNIVERSITY OF ILLINOIS OR THE ERNEST ORLANDO LAWRENCE BERKELEY NATIONAL LABORATORY. ENERGYPLUS IS A TRADEMARK OF THE US DEPARTMENT OF ENERGY.</small></p>
<p style="page-break-after:always;"></p>
<div id="generated-toc"></div>
<p style="page-break-after:always;"></p>


Overview
========

Document Overview
-----------------

This document is organized to give you the best possible look into the EnergyPlus calculations. First, the concepts of modeling in EnergyPlus are presented. These include descriptions of the zone heat balance process, air loop/plant loop processes as well as other important processes for the building simulation.

Discussions during the modeling process may reference specific “object names” as found in the Input/Output Reference document.

The remainder of the document focuses on individual models.

General Modeling Overview
-------------------------

The EnergyPlus program is a collection of many program modules that work together to calculate the energy required for heating and cooling a building using a variety of systems and energy sources. It does this by simulating the building and associated energy systems when they are exposed to different environmental and operating conditions. The core of the simulation is a model of the building that is based on fundamental heat balance principles. Since it is relatively meaningless to state: “based on fundamental heat balance principles”, the model will be described in greater detail in later sections of this document in concert with the FORTRAN code which is used to describe the model. It turns out that the model itself is relatively simple compared with the data organization and control that is needed to simulate the great many combinations of system types, primary energy plant arrangements, schedules, and environments. The next section shows this overall organization in schematic form. Later sections will expand on the details within the blocks of the schematic.

![](media/image1.png)

Figure 1. EnergyPlus Program Schematic

Simulation Manager
------------------

The simulation manager of EnergyPlus is contained in a single module. The main subroutine is shown below. Flow within the entire program is managed using a series of flags. These paired flags, in order (from the highest to the lowest) are:

Table 1. Simulation Flags

<table class="table table-striped">
<tr>
<td>BeginSimulationFlag</td>
<td>EndSimulationFlag</td>
</tr>
<tr>
<td>BeginEnvironmentFlag</td>
<td>EndEnvironmentFlag(one to many days)</td>
</tr>
<tr>
<td>BeginDayFlag</td>
<td>EndDayFlag</td>
</tr>
<tr>
<td>BeginHourFlag</td>
<td>EndHourFlag</td>
</tr>
<tr>
<td>BeginTimeStepFlag</td>
<td>EndTimeStepFlag</td>
</tr>
</table>

There is also a **WarmupFlag** to signal that the program is in warmup state. The operation of these flags can be seen in the following subroutine. The advantage of using the flag system is that any subroutine throughout the code can determine the exact state of the simulation by checking the status of the flags.

````
SUBROUTINE ManageSimulation     ! Main driver routine for this module
BeginSimFlag = .TRUE.
  EndSimFlag = .FALSE.
  CALL OpenOutputFiles
  CALL GetProjectData
  CALL GetEnvironmentInfo                 ! Get the number and type of Environments
  DO Envrn = 1, NumOfEnvrn        ! Begin environment loop ...
    BeginEnvrnFlag = .TRUE.
    EndEnvrnFlag   = .FALSE.
    WarmupFlag     = .TRUE.
    DayOfSim       =  0
    DO WHILE ((DayOfSim.LT.NumOfDayInEnvrn).OR.(WarmupFlag))  ! Begin day loop ...
      DayOfSim     = DayOfSim + 1
      BeginDayFlag = .TRUE.
      EndDayFlag   = .FALSE.
      DO HourOfDay = 1, 24      ! Begin hour loop ...
        BeginHourFlag = .TRUE.
        EndHourFlag   = .FALSE.
          DO TimeStep = 1, NumOfTimeStepInHour  ! Begin time step (TINC) loop ...
          BeginTimeStepFlag = .TRUE.
          EndTimeStepFlag   = .FALSE.
          ! Set the End\_\_Flag variables to true if necessary. Note that each flag builds on
          ! the previous level. EndDayFlag cannot be .true. unless EndHourFlag is also .true., etc.
          ! Note that the EndEnvrnFlag and the EndSimFlag cannot be set during warmup.
          ! Note also that BeginTimeStepFlag, EndTimeStepFlag, and the
          ! SubTimeStepFlags can/will be set/reset in the HVAC Manager.
          IF ((TimeStep.EQ.NumOfTimeStepInHour)) THEN
            EndHourFlag = .TRUE.
            IF (HourOfDay.EQ.24) THEN
              EndDayFlag = .TRUE.
              IF ((.NOT.WarmupFlag).AND.(DayOfSim.EQ.NumOfDayInEnvrn)) THEN
                EndEnvrnFlag = .TRUE.
                IF (Envrn.EQ.NumOfEnvrn) THEN
                  EndSimFlag = .TRUE.
                END IF
              END IF
            END IF
          END IF
          CALL ManageWeather
          CALL ManageHeatBalance
          BeginHourFlag  = .FALSE.
          BeginDayFlag   = .FALSE.
          BeginEnvrnFlag = .FALSE.
          BeginSimFlag   = .FALSE.
        END DO                              ! ... End time step (TINC) loop.
      END DO                    ! ... End hour loop.
    END DO                      ! ... End day loop.
  END DO                        ! ... End environment loop.
  CALL CloseOutputFiles
  RETURN
END SUBROUTINE ManageSimulation
````

Warmup Convergence
------------------

Since everything in EnergyPlus is based on the foundation of the loads simulation, it stands to reason that any inaccuracies in the loads calculation will result in inaccuracies of similar or larger magnitude in the HVAC calculations. In the presumably limited cases where convergence was not truly achieved before the actual simulation began, it is unknown how much error would be introduced into the results. While simulations that last longer (annual vs. design day) will hopefully have any initial condition problems balanced by the shear number of days in the simulation, shorter simulations—particularly those used for sizing—could result in relatively large errors. The simulation results could be unreliable and inaccurate when steady periodic conditions are not achieved. Therefore, it is important to properly determine when there is enough temperature and flux history terms to start an EnergyPlus simulation since this has a potential economic and energy impact on buildings that use EnergyPlus in design.

EnergyPlus determines warmup convergence in the following manner as shown in the Figure 2 below. The process of the convergence checks begins by tracking four parameters such including the maximum zone air temperature, the minimum zone air temperature, the maximum heating load, and the maximum cooling load for individual zone. It is note that these convergence checks are only in effective in simulations with at least one zone since the criteria is solely based on the maximum and minimum values obtained from an individual zone. Differences in these parameters between two consecutive days are then compared with the convergence tolerance values at the end of the day during the warmup period. For example, the maximum and minimum air temperature and the percentage difference of zone load for each zone at 9:00AM during the second to last warmup is compared to the values at 9:00AM last warmup day as follows:

<div>$${T_{\max ,prev}} - {T_{\max }} &lt; {T_{tol}}$$</div>

<div>$${T_{\min ,prev}} - {T_{\min }} &lt; {T_{tol}}$$</div>

<div>$$\frac{{{q_h} - {q_{h,prev}}}}{{{q_h}}} &lt; {q_{tol}}$$</div>

<div>$$\frac{{{q_c} - {q_{c,prev}}}}{{{q_c}}} &lt; {q_{tol}}$$</div>

where Tmax,prev is the maximum zone temperature of previous day, Tmax is the maximum zone temperature of current day, Ttol is the value of temperature tolerance, qh,prev is the maximum heating load of previous day, qh, is the maximum heating load of current day, qtol is the value of load tolerance, qc,prev is the maximum cooling load of previous day, and qc, is the maximum cooling load of current day.

Note that a minimum load of 100W is used to establish a fraction for the maximum loads when they are less than the minimum. This is done to avoid a false negative indication for the percentage load difference that may appear when zonal loads are very small. The convergence checks are repeated until passed for all zones. EnergyPlus assumes that the warmup period has been reached steady-periodic when these four parameters are within tolerance. Finally, temperature and load differences between the last two warmup days for individual zone at each time step in the last warmup day are reported so that users can easily track whether or not the warmup period has converged. The input parameters and output related to the warmup period are discussed in the Input-Output Reference.

![](media/image6.png)

Figure 2. Flows of Warmup Convergence Checks

