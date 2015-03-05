# Warmup Convergence 

Since everything in EnergyPlus is based on the foundation of the loads simulation, it stands to reason that any inaccuracies in the loads calculation will result in inaccuracies of similar or larger magnitude in the HVAC calculations. In the presumably limited cases where convergence was not truly achieved before the actual simulation began, it is unknown how much error would be introduced into the results. While simulations that last longer (annual vs. design day) will hopefully have any initial condition problems balanced by the shear number of days in the simulation, shorter simulations—particularly those used for sizing—could result in relatively large errors. The simulation results could be unreliable and inaccurate when steady periodic conditions are not achieved. Therefore, it is important to properly determine when there is enough temperature and flux history terms to start an EnergyPlus simulation since this has a potential economic and energy impact on buildings that use EnergyPlus in design.

EnergyPlus determines warmup convergence in the following manner as shown in the Figure 2 below. The process of the convergence checks begins by tracking four parameters such including the maximum zone air temperature, the minimum zone air temperature, the maximum heating load, and the maximum cooling load for individual zone. It is note that these convergence checks are only in effective in simulations with at least one zone since the criteria is solely based on the maximum and minimum values obtained from an individual zone. Differences in these parameters between two consecutive days are then compared with the convergence tolerance values at the end of the day during the warmup period. For example, the maximum and minimum air temperature and the percentage difference of zone load for each zone at 9:00AM during the second to last warmup is compared to the values at 9:00AM last warmup day as follows:

![](media/image2.png)\


![](media/image3.png)\


![](media/image4.png)\


![](media/image5.png)\


where Tmax,prev is the maximum zone temperature of previous day, Tmax is the maximum zone temperature of current day, Ttol is the value of temperature tolerance, qh,prev is the maximum heating load of previous day, qh, is the maximum heating load of current day, qtol is the value of load tolerance, qc,prev is the maximum cooling load of previous day, and qc, is the maximum cooling load of current day.

Note that a minimum load of 100W is used to establish a fraction for the maximum loads when they are less than the minimum. This is done to avoid a false negative indication for the percentage load difference that may appear when zonal loads are very small. The convergence checks are repeated until passed for all zones. EnergyPlus assumes that the warmup period has been reached steady-periodic when these four parameters are within tolerance. Finally, temperature and load differences between the last two warmup days for individual zone at each time step in the last warmup day are reported so that users can easily track whether or not the warmup period has converged. The input parameters and output related to the warmup period are discussed in the Input-Output Reference.

![Flows of Warmup Convergence Checks](media/flows-of-warmup-convergence-checks.png)
