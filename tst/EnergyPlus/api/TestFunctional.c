#include <stdio.h>
#include <EnergyPlus/api/functional.h>

int main() {
    CBaseThermalPropertySet *props = NULL;
    props = newCBaseThermalPropertySet(1.0, 2.0, 3.0);
    Real64 diffusivity = cBaseThermalPropertySet_diffusivity(props);
    printf("C API Test: Calculated thermal diffusivity: %8.4f \n", diffusivity);
    cBaseThermalPropertySet_setConductivity(props, 4.0);
    diffusivity = cBaseThermalPropertySet_diffusivity(props);
    printf("C API Test: Updated thermal diffusivity: %8.4f \n", diffusivity);
    delCBaseThermalPropertySet(props);
    return 0;
}
