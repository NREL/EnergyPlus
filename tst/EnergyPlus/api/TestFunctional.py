from pyenergyplus.api import EnergyPlusAPI


api = EnergyPlusAPI()
functional_api = api.functional()
glycol = functional_api.glycol(u"water")

for t in [5.0, 15.0, 25.0]:
    cp = glycol.specific_heat(t)
    rho = glycol.density(t)
    k = glycol.conductivity(t)
    visc = glycol.viscosity(t)
    print("Python API Test: Calculated properties at T=%s: %f, %f, %f, %f" % (t, cp, rho, k, visc))
