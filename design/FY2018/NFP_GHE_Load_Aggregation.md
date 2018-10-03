Enhanced Load Aggregation Method for Ground Heat Exchangers
===========================================================

**Matt Mitchell, OSU**

- Original Date: 2018-10-03

## Justification for New Feature ##

The EnergyPlus ground heat exchanger (GHE) model is based on a response factor approach which relies on a series of pre-computed response factors--a.k.a. "g-functions"--to compute the GHE temperature response. To compute the the GHE temperature response at any given timestep, the response factors along with the GHE load history are combined together as indicated in the equation below.

$T_f = T_g + \sum_{i=1}^n \frac{q_{i} - q_{i-1}}{2 \cdot \pi \cdot k_g} \cdot g\left(\frac{t_n - t_{i-1}}{t_s}\right) +  q_n \cdot R_b$

where:

$T_f$ is the current GHE mean-fluid temperature

$T_g$ is the ground temperature

$q_i$ is the $i^{th}$ GHE heat transfer rate normalized based on GHE length. i.e Btu/h-ft, W/m 

$t_i$ is the $i^{th}$ timestep

$t_s$ is the GHE time constant $\frac{H^2}{9\cdot\alpha_g}$

$R_b$ is the borehole thermal resistance

$g$ is the g-function response factors, which are generates previously by EnergyPlus or third-party tools.

Conduction can be described using linear partial differential equations. Because no non-linear terms exist, the principle of superpostiion can be applied to conduction problems. GHE response factor models take advantage of this and apply the principle of superposition to compute the GHE temperature response by using the GHE load history. This series of heat pulses, when superimposed and combined using the response factors, can lead to an accurate calculation of the current GHE temperature response.

A practical issue that arises from this superpostion approach, however, is the fact that the number of superposition calculations grows with the square of the number of timesteps. Therefore, this approach greatly affects simulation runtime. Attempting to simulate, say, for example for annual or multi-year simulations may be impractical without some way to reduce the number of computations required.

In order to reduce runtime, load aggregation procedures have been developed which will reduce then number of superposition calculations, which in turn results in reduced runtime. The method currently implemented in EnergyPlus relies on the original load aggregation method developed by Yavuzturk & Spitler (1999), however, since this method's development and implementation a number of more advanced methods have been developed.

This project proposes to test the currently available methods and implement the best approach.

## E-mail and Conference Call Conclusions

N/A

## Overview

Development of EnergyPlus must necessarily be considerate of simulation runtime. Many thousands of users download and use the program each year. These users, as well as additional third-party tools rely on EnergyPlus be and not only accurate but as fast as possible.

This project proposes to test, validate, and implement load aggregation methods for the ground heat exchanger model with the aim of improving accuracy and reducing model runtime.

## Approach

The code for this project has been prototyped, developed, optimized, and tested outside of the EnergyPlus repository.  The prototype code can be found here: https://github.com/mitchute/GLHE

The code was developed in Python to simplify the implementation and prototyping process. All of the load aggregation methods which were tested are described below. A parametric study was performed using the Cowboy Cluster at OSU, which is supported by NSF MRI award OCI-1126330.

#### Yavuzturk & Spitler 1999

Yavuzturk & Spitler (1999) were the first to develop a load aggregation procedure to reduce simulation runtime. The method described by Yavuzturk & Spitler (1999) relies on using "monthly" heat pulses to aggregate the loads. The method retains 192 hourly heat pulses for the most recent 192 simulation hours. Once the first 922 (192 + 730) hours have passed, the 730 hours which are farthest from the current simulation time are aggregated into monthly blocks. Once another 730 hours have passed the next 730 hours are aggregated into another monthly block. This process is repeated for the duration of the simulation.

Once a simulation is initiated the GHE hourly load history begins to be stored. For example, once 922 (192 + 730) simulated hours have passed, the hourly loads from hour 729 to 0 are aggregated together into a single block. The mean value of the loads over this period is computed and the 730 individual hourly loads are removed and replaced with a single value. This single value represents the average load over this 730 hour period. As the simulation continues for another 730 hours until hour 1652, the hourly loads from hours 1459 to 730 are aggregated together again into another monthly block.

This method forms the basis for what is termed the "static" method. The method is characterized by smaller load blocks which collapse into larger blocks once a sufficient number of the smaller blocks have been created. This was illustrated in the above example when the smaller hourly load blocks collapsed into the larger 730 hour blocks after a sufficient number of smaller blocks have been created. However, the the optimum number and duration of each block has yet to be determined. 

The method could loosely be thought of as similar to the Lagrangian approach which, in the case of fluid flow characterizations, tracks individual particles or packets of fluid, rather than tracking a fixed control volume through which fluid flows. In our case, however, we are concerned with tracking individual loads, or load blocks which are formed from previously aggregated smaller blocks.

The Yavuzturk & Spitler method is labeled as "Monthly" in the subsequent plots.

Data sets identified as "Static" apply the same logic, but the parameters regarding the minimum number of bins and the size of each bin are varied systematically in a parametric study.

#### Bernier et al. (2004)

The method developed by Bernier et al. (2004) is another approach which is very similar to the Yavuzturk & Spitler (1999) method, however the number and duration of the load aggregation intervals is different. 

In this case, the authors describe using 12 single hours for the 12 most recent hours of the simulation. After that, the loads begin to aggregate into 48, 168, and 360 hour blocks.

This method is labeled as "MLAA" in the subsequent plots. 

#### Liu (2005)

Liu (2005) developed another method which he called the "Hierarchical" method. This method represents a single case within the overarching "static" method. The minimum number and duration of each bin is outlined below.

- A minimum of 12 most recent hourly load values.
- Hourly loads are aggregated into 24 hours blocks.
- Once eight, 24 hour blocks have been created the 5 which are most-distant from the current simulation time are aggregated into a single block. The remaining three blocks are kept aggregated at the 24 hour level.
- Once 103, 120 hour (24 x 5) blocks have been created, the 73 which are most distant from the current simulation time are aggregated into a single block. This process is repeated as required.

This method is labeled as "Hierarchical" in the subsequent plots.

#### Claeson & Javed (2012)

The method developed by Claesson & Javed (2012) is the most recent load aggregation method which has been developed. The method, as opposed to the "static" method described previously, follows what could be characterized as a Eulerian approach. In a fluid flow application, the flow through a fixed control volume is assessed rather than individual packets or particles of fluid.

In our case, we predefine the bins and allow energy (GHE loads) to move through the bins.  This is illustrated in the table below, which is an example taken from Claesson & Javed (2012).

In the table the values of the GHE loads for each timestep within each bin is represented. Horizontally, the bin widths are are given as h or 2h, where h represent the length of the timestep. The most recent load is always in the first bin. See the following example description.

- At timestep 1, a value of 1 is placed in the Bin 1. 
- At timestep 2, h  x 1/h x Bin 1 is moved from Bin 1 to Bin 2. In other words, the timestep length, times the inverse of the current bin width, times the value of the energy in bin itself is moved from Bin 1 to Bin 2. In this case, the timestep is h, the bin width is h, and the value of the energy in Bin 1 is 1, so 100% of the energy (1) is moved from Bin 1 to Bin 2.  Energy is conserved and since the bins have the same width (meaning they represent the same time interval), the new value of Bin 2 is 1.
- At timestep 3, the same process is repeated. h x 1/h x 1 is the value of energy which is moved from Bin 2 to Bin 3. However, Bin 3 has a width of 2h, so the magnitude of the energy represented by the Bin is now 1/2. Again, energy is conserved but the original pulse of 1 is now assumed to have occurred over a 2h time interval. This process is repeated for all remaining timesteps.

Table 1: Dynamic method example.

|              | Bin 1 | Bin 2 | Bin 3 | Bin 4 | Bin 5 | Bin 6 | Bin 7 |
| ------------ | ----- | ----- | ----- | ----- | ----- | ----- | ----- |
| Timestep $n$ | h     | h     | 2h    | 2h    | 2h    | 2h    | 2h    |
| 1            | 1     |       |       |       |       |       |       |
| 2            | 0     | 1     |       |       |       |       |       |
| 3            | 0     | 0     | 1/2   |       |       |       |       |
| 4            | 0     | 0     | 1/4   | 1/4   |       |       |       |
| 5            | 0     | 0     | 1/8   | 2/8   | 1/8   |       |       |
| 6            | 0     | 0     | 1/16  | 3/16  | 3/16  | 1/16  |       |
| 7            | 0     | 0     | 1/32  | 4/32  | 6/32  | 4/32  | 1/32  |

This method is labeled as "Dynamic" in the subsequent plots.

## Testing/Validation

The methods were tested by performing a parametric study which swept a wide range of variations for for the input parameters. The two primary methods being tested are the dynamic and static method. Any other methods listed are subsets of the static method.

EnergyPlus was used to generate two sets of annual loads. One load set consists of balanced heating and cooling loads, and the other set consists of imbalanced heating and cooling loads.  The simulations were also ran for 1 and 5 years for each of the load cases, resulting in a total of four simulations for each variation of the parametric study.

The simulations were compared against simulations for each of the four load and simulation variations using no load aggregation method. The results from these simulations gives the baseline for comparing the results of the parametric runs.

Figures 1 and 2 below show the single-year balanced and imbalanced simulation results. The plots show the fraction of the non-aggregated simulation time on the vertical axis. The root-mean squared error of the mean GHE fluid temperature is plotted on the horizontal axis.

In both cases, the dynamic method out-performs the static method in terms of simulation time reduction and in terms of accuracy. 

![1-Year Balanced Load Results](balanced_1_fraction.png)

Figure 1: 1-year balanced load parametric results.



![1-Year Imbalanced Load Results](imbalanced_1_fraction.png)

Figure 2: 1-year imbalanced load parametric results.

Therefore, based on the above results, implementation of the dynamic method is proposed.

## I/O Reference Documentation

No changes

## Engineering Reference Documentation

A few paragraphs describing the selected enhanced load aggregation procedure. A paper will be drafted regarding the work. This should also be referenced here.

## Transition/Changes

None

## References

Bernier, M. A., P. Pinel, R. Labib and R. Paillot. 2004. 'A multiple load aggregation algorithm for annual hourly simulations of GCHP systems.' *HVAC&R Research*, 10:4, 471--487.

Claesson, G. and Saqib Javed. 2012. 'A load-aggregation method to calculate extraction temperatures of borehole heat exchangers.' In proceedings of the ASHRAE Winter Conference, Chicago, IL.

Liu. X. 2005. Development and experimental validation of simulation of hydronic snow melting systems for bridges. Ph.D. Thesis. Oklahoma State University, Stillwater, OK.

Yavuzturk, C. and J. D. Spitler. 1999. 'A short time step response factor model for vertical ground loop heat exchangers.' *ASHRAE Transactions*, 105:2, 475--485.

