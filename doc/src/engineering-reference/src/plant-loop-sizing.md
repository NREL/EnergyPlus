# Plant Loop Sizing

## Introduction

The program needs to be able to autosize the fluid flow rate in each plant fluid loop. The design plant loop flow rates are set by the sum of the needs of the demanding components on each loop. For chilled water loops these components will be cooling coils. For hot water loops – hot water coils. And for condenser loops – various types of chiller that use condenser water for cooling. Each component that uses water for heating or cooling stores its design water flow rate (in its sizing routine) in the array *CompDesWaterFlow*, labeled by its inlet water supply node number. These individual component design water flow rates are then accessed, summed for each plant loop, and stored in the *PlantSizingData* array. This array also contains the user specified design values for each plant loop.

## Hot and Chilled Water Loop Sizing

### Maximum Loop Volumetric Flow Rate

The loop maximum volumetric flow rate (m^3^) is just set equal to the value stored in the *PlantSizData* array for this loop.

### Volume of the plant loop

Since the loop capacitance has a stability requirement of ![](media/image1985.png)  the volume is set so that the stability requirement will be 0.8 at the zone time step, which is the largest time step encountered at the max flow rate the loop can reach.

![](media/image1986.png)\


## Condenser Loop Sizing

### Maximum Loop Volumetric Flow Rate

The loop maximum volumetric flow rate (m^3^) is just set equal to the value stored in the *PlantSizData* array for this loop.

### Volume of the plant loop

Since the loop capacitance has a stability requirement of ![](media/image1987.png)  the volume is set so that the stability requirement will be 0.8 at the zone time step, which is the largest time step encountered at the max flow rate the loop can reach.

![](media/image1988.png)\
