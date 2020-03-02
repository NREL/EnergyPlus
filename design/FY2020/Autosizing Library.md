A New Autosizing Library for EnergyPlus
=======================================

A major part of EnergyPlus is the ability to autosize components and systems.
The logic, algorithms, and coefficients used for doing this sizing is partially captured in a large `RequestSizing` function, but there is also some logic that is spread throughout the code, and some blocks are repeated in many locations.

This work will organize the existing autosizing capabilities into a well-organized source code subdirectory `src/EnergyPlus/Autosizing`.
In this folder, a base class will be created which defines the capabilities for all possible autosized values.
The final form of this base class is not yet locked down, but it will presumably contain at least this:

```c++
class BaseSizer {
public:
    Real64 originalValue;
    Real64 autosizedValue;
    bool wasAutosized;
}
```

Then derived types will be created from this base class, and each derived type will contain the algorithms, logic, and coefficients, necessary for sizing that value.
However, the static data is not the only thing needed for all sizing, some autosizing also needs dynamic load data to evaluate sizes.

A possible autosizing derived class could look like the following:

```c++
// declaration
class ChilledWaterCoilFlowSizer : BaseSizer {
private:
    constexpr coefficientAlpha = 3.14159;
public:
    ChilledWaterCoilFlowSizer();
    bool size(Real64 _originalValue, Real64 fixedValue1, Real64 fixedValue2, std::vector<Real64> dynamicLoadData);
}

// definition
bool ChilledWaterCoilFlowSizer::size(Real64 _originalValue, Real64 fixedValue1, Real64 fixedValue2, std::vector<Real64> dynamicLoadData) {
    // sizes chilled water coil water flow rates
    this->originalValue = _originalValue;
    if (_originalValue == DataSizing::Autosize) {
        this->wasAutosized = true;
    }
    // sizing etc
    this->autosizedValue = fixedValue1 + fixedValue2 * dynamicLoadData[0];
    return true;
}
```

The `size(...)` method will not have consistent arguments across all types and this is just fine.
We aren't using the base class approach in order to gain polymorphism, we are using it to organize our data.

Now, each component in EnergyPlus that has an autosized value would just hold an instance of this derived sizer class.
The component can call the size(...) function, retrieve the autosized value, and also inspect the instance to get more data if needed.

The goal of this will be to make the size() functions totally portable -- no global data access.
If the size() method needs a value, pass in a reference to it.
Testing will be amazing -- we should be able to easily test each derived type by calling size() with a series of values.

Things to still consider:
 - How error handling is handled
 - Which autosizing values to start with -- the least used I'd say
