Enhanced Load Aggregation Method for Ground Heat Exchangers - Design
==========================

**Matt Mitchell, OSU**

- October 3, 2018

# Overview #
This document outlines the code design for the enhanced GHE load aggregation algorithm. The code replaces existing code, and therefore no new modules are expected. The GHE code has largely been refactored in recent years, so no major modifications are expected.

# New Classes #

One new class is expected, which is a simple container class for the aggregated loads. It is not expected to have any methods. It should be similar to the following:

```c++
class LoadAggBin(float energy=0, int width=0) {
    float energy;
    float width;
    
    LoadAggBin():
    	energy(0),
    	width(0)
    {}
}
```



# Modifications to Existing Functions

The load aggregation algorithm is self-contained (mostly) within the ```GroundHeatExchangers.cc``` file, under the module ```calcAggregateLoad()``` . This function will be modified to apply the load aggregation method specified in the NFP. Some other minor changes are expected, to reduce the number of variables since the current code stores the sub-hourly, hourly, and monthly heat pulse value in separate arrays. These will necessarily collapse into a single array, therefore some minor other changes are expected to accommodate this.