A New Autosizing Library for EnergyPlus
=======================================

A major part of EnergyPlus is the ability to autosize components and systems.
The logic, algorithms, and coefficients used for doing this sizing is partially captured in a large `RequestSizing` function, but there is also some logic that is spread throughout the code, and some blocks are repeated in many locations.

# Overview

This work will organize the existing autosizing capabilities into a well-organized source code subdirectory `src/EnergyPlus/Autosizing`.
In this folder, a base class will be created which defines the capabilities for all possible autosized values.
The final form of this base class is not yet locked down, but it will presumably contain at least this:

```c++
enum class SizingStatus { Success, Error }
struct BaseSizer {
    Real64 originalValue;
    Real64 autosizedValue;
    bool wasAutosized;
    virtual void init(...) = 0;
    virtual SizingStatus size(...) = 0;
}
```

Derived types will be created from this base class, and each derived type will contain the algorithms, logic, and coefficients, necessary for sizing that value.
However, the static data is not the only thing needed for all sizing, some autosizing also needs dynamic load data to evaluate sizes.

A possible autosizing derived class could look like the following:

```c++
// declaration
struct ChilledWaterCoilFlowSizer : BaseSizer {
    constexpr coefficientAlpha = 3.14159;
    ChilledWaterCoilFlowSizer();
    void init(...) override;
    SizingStatus size(...) override;
}

// definition
bool ChilledWaterCoilFlowSizer::size(...) {
    // sizes chilled water coil water flow rates
    this->originalValue = ...
    if (_originalValue == DataSizing::Autosize) {
        this->wasAutosized = true;
    }
    // sizing etc
    this->autosizedValue = fixedValue1 + fixedValue2 * dynamicLoadData[0];
    return SizingStatus::Success;
}
```

The `init(...)` and `size(...)` methods will try to use unified arguments across all types to achieve additional gains, but if not then we'll work around that.

Each component in EnergyPlus that has an autosized value would just create an instance of a derived sizer class.
The component can call the size(...) function, retrieve the autosized value, and also inspect the instance to get more data if needed.
In the long run, once we have these sizers fully organized, we may be able to leverage polymorphism and allow components to store an intermediate class, but this is not important immediately.

# Implementation

The goal of this will be to make the size() functions totally portable -- no global data access.
However, this will require a massive amount of data to be passed in or set up.
The hope is that ultimately these sizer functions can be exposed via the API, which would make this situation more awkward.
To allow this to be implemented throughout the code in a reasonable amount of time, we will make the size() functions fully portable, and instead rely on an initialization function to bring in data.
At first, the sizer classes will have just one initialization function, akin to `initializeFromEPlus()`.
The purpose of this function is to bring all EnergyPlus "global" data into the struct members.
This will enable the actual call to `size` to be portable, without a dependence on global data.
Once we start exposing this via API, we will add a new function called `initializeFromAPI`, which will require the user to pass in all the required data that would otherwise be setup inside EnergyPlus.
This approach has a benefit of generally being able to stay out of the way of other refactors, including the global state refactor currently underway.

The base class will contain as much information and functionality as possible in order to reduce the burden on each individual sizer class.
Although the final form will evolve a little over time, we anticipate the base class to have its own `initializeFromEPlus` function, as well as `preSize` and `report` functions that derived types must call.

# Testing

Unit testing will be vitally important during this.
From the very beginning we will attempt to achieve 100% unit test coverage by calling size() with varying initialization.

# Expected Changes

This feature will have zero input changes, and expected to have zero (or at least *extremely* small) diffs.
There's really no reason for diffs unless something is fixed along the way.

# Process

One beauty of this method is that sizers can be added incrementally and component models can easily switch between the legacy and new implementations in case diffs are encountered.
We will determine as we go which sizer to implement each step of the way, and make revisions to the implementation if function arguments need to change in the base class.
The actual order doesn't matter too much as we will have converted all of them by the end of this project anyway.

# API

Now, once we have a pattern nailed down and approved, we will start considering the design of the API.
The basic idea is that we will need to wrap the C++ classes with C interfaces, and then create Python bindings on top of those.
We won't do this at first because we will want to simplify and solidify the interface and required data and arguments as much as possible first.
