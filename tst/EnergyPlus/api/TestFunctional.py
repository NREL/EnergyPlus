import os
import sys
sys.path.append(os.path.dirname(os.path.realpath(__file__)))

from energyplus_api import EnergyPlusAPI


api = EnergyPlusAPI()

props = api.props(1.0, 2.0, 3.0)
d = props.diffusivity()
print("Python API Test: Calculated thermal diffusivity = " + str(d))

props.set_conductivity(4)
d = props.diffusivity()
print("Python API Test: Updated thermal diffusivity = " + str(d))
