from pyenergyplus.api import EnergyPlusAPI


api = EnergyPlusAPI()
functional_api = api.functional()
thermal_props = functional_api.base_struct(1.0, 2.0, 3.0)

d = thermal_props.diffusivity()
print("Python API Test: Calculated thermal diffusivity = " + str(d))
thermal_props.set_conductivity(4)
d = thermal_props.diffusivity()
print("Python API Test: Updated thermal diffusivity = " + str(d))

fluid_properties = functional_api.fluid_properties("STEAM")
sat_press = fluid_properties.get_sat_press_refrigerant()
print("Python API Test: Saturation pressure = " + str(sat_press))
