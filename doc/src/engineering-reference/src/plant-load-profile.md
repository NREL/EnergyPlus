# Plant Load Profile

The LoadProfile:Plant object is used to simulate a scheduled demand profile. This can be useful when the building loads are already known. Demanded load and flow rate are schedules specified in the object definition. The load profile can specify heating and cooling loads. Cooling loads are entered as negative numbers. The actual load met is dependent on the performance of the supply loop components.

The LoadProfile:Plant object must be connected on the demand side of the plant loop. If desired, multiple LoadProfile:Plant objects can be combined in series and/or parallel.

## Calculation Model

The LoadProfile:Plant object calculates the outlet water temperature based on the inlet water temperature from the plant loop and user inputs for the scheduled plant load and the requested flow rate.  The calculation can be expressed with the equation:

![](media/image1890.png)\


where

![](media/image1891.png)  = the outlet water temperature

![](media/image1892.png)  = the inlet water temperature

![](media/image1893.png)  = the scheduled plant load

![](media/image1894.png)  = the water mass flow rate

![](media/image1895.png)  = the specific heat of water

The user requested flow rate is not always available from the plant loop.  The actual flow rate used in the calculation is the lesser of the user requested value and the plant available value.

Note that the LoadProfile:Plant object can still request and receive flow even if the scheduled plant load is zero.  In this case the outlet temperature will be the same as the inlet temperature.  This allows users to drive the plant loop flow without necessarily affecting the loop temperature.

For reporting purposes the energy consumption of the object is calculated using the equation:

![](media/image1896.png)\


where

![](media/image1897.png)  = the energy consumption

![](media/image1898.png)  = the scheduled plant load

![](media/image1899.png)  = the time step interval