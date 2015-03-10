# CentralHeatPumpSystem 

## Overview

**The** CentralHeatPumpSystem object simulates the performance of a central heat pump system containing one or more chiller-heater objects. The main function of the object is to call relevant calculation methods for the chiller-heater objects depending on their operating modes, and to calculate temperatures at the outlet nodes and the total energy transfer. The object can be connected to plant loops and a source loop (see Input-Output Reference document), and the node connections are solely dependent on individual chiller-heaters' operating modes. The central system receives water flows from each plant and source loop and then distributes them to individual chiller-heaters as requested. The conditioned water flows leaving the chiller-heaters are then returned to the system, and in turn flow back to the plant loops and source loop to produce heating and cooling, or exchange heat with the source loop.

## Model Description

The model first determines operating modes based on the cooling and heating loads on the system, and then decides which calculation algorithm is called. It calls the cooling calculation subroutine when cooling load is demanded, the heating calculation subroutine when heating load is demanded, and both cooling and heating calculation subroutines when both cooling and heating loads are demanded, i.e., simultaneous cooing-heating mode. It then calculates mass-weighed temperatures and heat transfer energy. Six different operating modes (0 through 5) are possible:

- 0: off
- 1: cooling-only mode
- 2: heating-only mode
- 3: heat recovery mode
- 4: cooling dominant simultaneous cooling-heating mode
- 5: heating dominant simultaneous cooling-heating mode.

The model reads different node information over the operating mode so that outlet water conditions at each node of the system can be appropriately determined.

In cooling-only mode (mode 1), the system is connected to the chilled water loop and source loop. It thus reads the evaporator outlet temperatures and mass flow rates of the chiller-heaters to calculate a mass-weighed chilled water temperature at the system outlet node, and the condenser outlet temperatures and mass flow rates of the chiller-heaters to calculate a mass-weighed source water temperature at the system outlet node. In heating-only mode (mode 2), the system is connected to the hot water loop and source loop. It thus reads the condenser temperatures and mass flow rates of the chiller-heaters to calculate a mass-weighed hot water temperature at the system outlet node, and the evaporator temperatures and mass flow rates of the chiller-heaters to calculate a mass-weighed source temperature at the system outlet node.

In simultaneous cooling-heating mode, three different operating modes (mode 3 through 5) are possible. The model checks which simultaneous cooling-heating mode (3, 4, or 5) each chiller-heater is in, and calculates relevant mass-weighed temperatures. The system may be connected to three loops such as chilled water loop, hot water loop, and source water loop in mode 4 or mode 5.

In heat recovery mode, both evaporator temperatures and condenser temperatures are read and a mass-weighed temperature for both chilled water and hot water is calculated. The chiller-heaters in mode 3 do not exchange heat with source water. At least one of the chiller-heaters within the system is in heat recovery mode during simultaneous cooling-heating mode. The system may be only connected to both chilled water loop and hot water loop if all operating chiller-heaters are in heat recovery mode.

The following nomenclature is used in the following equations:

*![](media/image5480.png)*  *= chilled water bypass mass flow rate in the system [kg/s]*

*![](media/image5481.png)* *= hot water bypass mass flow rate in the system [kg/s]*

*![](media/image5482.png)*  *=* source water bypass mass flow rate in the system [kg/s]

![](media/image5483.png)  = chilled water mass flow rate of i^th^ chiller-heater's evaporator [kg/s]

*![](media/image5484.png)*  *=* hot water mass flow rate of i^th^ chiller-heater's condenser [kg/s]

*![](media/image5485.png)*  *=* source water mass flow rate of i^th^ chiller-heater, which varies with operating modes [kg/s]

*![](media/image5486.png)*  *=* chilled water mass flow rate of the system [kg/s]

*![](media/image5487.png)*  *=* hot water mass flow rate of the system [kg/s]

*![](media/image5488.png)*  *=* source water mass flow rate of the system [kg/s]

*![](media/image5489.png)*  *=* chilled water outlet temperature of the system [C]

*![](media/image5490.png)*  *=* hot water outlet temperature of the system [C]

*![](media/image5491.png)*  *=* source water outlet temperature of the system [C]

*![](media/image5492.png)*  *=* mass-weighed bypass chilled water temperature in the system [C]

*![](media/image5493.png)*  *=* mass-weighed bypass hot water temperature in the system [C]

*![](media/image5494.png)*  *=* mass-weighed bypass source water temperature in the system [C]

*![](media/image5495.png)*  *=* mass-weighed chilled water outlet temperature from chiller-heaters [C]

*![](media/image5496.png)*  *=* mass-weighed hot water outlet temperature from chiller-heaters [C]

*![](media/image5497.png)*  *=* mass-weighed source water outlet temperature from chiller-heaters [C]

*![](media/image5498.png)*  *=* chilled water inlet temperature of the system [C]

*![](media/image5499.png)*  *=* hot water inlet temperature of the system [C]

*![](media/image5500.png)*  *=* source water inlet temperature of the system [C]

*![](media/image5501.png)*  *=* chilled water outlet temperature of i^th^ chiller-heater [C]

*![](media/image5502.png)*  *=* hot water outlet temperature of i^th^ chiller-heater [C]

*![](media/image5503.png)*  *=* source water outlet temperature of i^th^ chiller-heater [C]

The model reads node information and local variables of individual chiller-heaters. The nodes and local variables vary with the operating modes as described above in order to calculate mass-weighed temperatures. In the cooling-only mode, it calculates a mass-weighed chilled water temperature (*T~cw,CH~*) and a source water temperature (*T~src,CH~*) as follows:

![](media/image5504.png)\


**![](media/image5505.png)**  ****.

In heating-only mode, it calculates a mass-weighed hot water temperature (*T~h~~w,CH~*) and a source water temperature (*T~src~~,CH~*) as follows:

**![](media/image5506.png)**  ****

**![](media/image5507.png)**  ****.

When all chiller-heaters are in heat recovery mode, it calculates a mass-weighed chilled water temperature (*T~cw,CH~*) and hot water temperature (*T~h~~w,CH~*) as follows:

**![](media/image5508.png)**

**![](media/image5509.png)**  ****.

In cooling or heating dominant simultaneous cooling-heating mode (mode 4 and 5), at least one chiller-heater should be in heat recovery mode, and the other(s) are in either mode 4 or mode 5. The system is connected to three loops such as chilled water loop, hot water loop, and source water loop. The model thus calculates a mass-weighed chilled water temperature (*T~cw,CH~*), hot water temperature (*T~h~~w,CH~*), and source water temperature (*T~src~~,CH~*) as follows:

**![](media/image5510.png)**

**![](media/image5511.png)**

**![](media/image5512.png)**  **.**

The model then calculates a mass-weighed temperature for the by-pass flows remained in the system as follows:

![](media/image5513.png)\


![](media/image5514.png)\


![](media/image5515.png)\


The outlet temperatures at each outlet node of the system are then determined as it sums both mass-weighed temperatures up as follows:

![](media/image5516.png)\


![](media/image5517.png)\


![](media/image5518.png) .

The total heat transfer energy of the system is also calculated in the same manner as the temperature calculations. The model simply sums all heat transfer energy of the chiller-heaters depending on their operating modes.