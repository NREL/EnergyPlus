import sys

from pyenergyplus.api import EnergyPlusAPI

api = EnergyPlusAPI()

state = api.state_manager.new_state()

# GLYCOL TESTS
glycol = api.functional.glycol(state, u"water")
for t in [5.0, 15.0, 25.0]:
    cp = glycol.specific_heat(state, t)
    rho = glycol.density(state, t)
    k = glycol.conductivity(state, t)
    viscosity = glycol.viscosity(state, t)
    print("Python API Test: Calculated properties at T=%s: %f, %f, %f, %f" % (t, cp, rho, k, viscosity))
glycol.delete(state)

# REFRIGERANT TESTS
refrigerant = api.functional.refrigerant(state, "steam")
temperature = 100.0
satPress = refrigerant.saturation_pressure(state, temperature)  # expecting about 101325 Pa
thisPress = 100000
satTemp = refrigerant.saturation_temperature(state, thisPress)  # expecting about 100 degC
print("Python API Test: Saturated Properties: At 100C, Psat=%8.4f; at 100000Pa, Tsat=%8.4f" % (satPress, satTemp))
satLiqDens = refrigerant.saturated_density(state, temperature, 0.0)  # // liq = 958 kg/m3
satLiqCp = refrigerant.saturated_specific_heat(state, temperature, 0.0)  # liq = 4,216 J/kgK
satLiqEnth = refrigerant.saturated_enthalpy(state, temperature, 0.0)
print("C API Test: Sat Liq at 100C: rho=%8.4f, Cp=%8.4f, h=%8.4f" % (satLiqDens, satLiqCp, satLiqEnth))
satVapDens = refrigerant.saturated_density(state, temperature, 1.0)  # vap = 1/1.6718 ~~ 0.59 kg/m3
satVapCp = refrigerant.saturated_specific_heat(state, temperature, 1.0)  # vap = 2,080 J/kgK
satVapEnth = refrigerant.saturated_enthalpy(state, temperature, 1.0)
print("C API Test: Sat Vap at 100C: rho=%8.4f, Cp=%8.4f, h=%8.4f" % (satVapDens, satVapCp, satVapEnth))
enthDifference = satVapEnth - satLiqEnth  # vap-liq = 2,675,570-419,170 ~ 2,256,400 J/kg
refrigerant.delete(state)

# PSYCHROMETRIC TESTS
psychrometrics = api.functional.psychrometrics(state)
# // PSYCHROMETRICS
# // test point is:
# //   Barometric Pressure: 101325 Pa
# //   Dry Bulb Temp: 24 C
# //   Relative Humidity: 0.5
# //   Humidity Ratio: ~0.009 kg/kg
# //   Saturation Temp: ~17 C
# //   Saturation Pressure: 2985 Pa
# //   Enthalpy: ~48000 J/kg
# //   Specific Volume: 0.855 m3/kg
# //   Density: ~1.17
# //   Wet Bulb: ~17 C
# //   Dew Point: ~13 C
# //   Vapor Density: 0.0107 kg/m3
# //   Specific Heat: ~1007 J/kgK
print("Python API Test: Psych props, test point is about 101325Pa, 24C, 50%% humidity:")
db = psychrometrics.dry_bulb(state, 48000, 0.009)
print("Python API Test: Expected DB ~ 24 C Calculated: %8.4f" % db)
rh = psychrometrics.relative_humidity(state, 24, 0.0107)
rh2 = psychrometrics.relative_humidity_b(state, 24, 0.009, 101325)
print("Python API Test: Expected RH ~ 0.5 Calculated: %8.4f, %8.4f" % (rh, rh2))
hr = psychrometrics.humidity_ratio(state, 24, 48000)
hr2 = psychrometrics.humidity_ratio_b(state, 13, 101325)
hr3 = psychrometrics.humidity_ratio_c(state, 24, 0.5, 101325)
hr4 = psychrometrics.humidity_ratio_d(state, 24, 17, 101325)
print("Python API Test: Expected HumRat ~ 0.009 Calculated: %8.4f, %8.4f, %8.4f, %8.4f" % (hr, hr2, hr3, hr4))
tSat = psychrometrics.saturation_temperature(state, 48000, 101325)
print("Python API Test: Expected Tsat ~ 17 C Calculated: %8.4f" % tSat)
pSat = psychrometrics.saturation_pressure(state, 24)
print("Python API Test: Expected Psat ~ 2985 Pa Calculated: %8.4f" % pSat)
h = psychrometrics.enthalpy(state, 24, 0.009)
h2 = psychrometrics.enthalpy_b(state, 24, 0.5, 101325)
print("Python API Test: Expected Enth ~ 0.48000 J/kg Calculated: %8.4f, %8.4f" % (h, h2))
volume = psychrometrics.specific_volume(state, 24, 0.009, 101325)
print("Python API Test: Expected v ~ 0.855 m3/kg Calculated: %8.4f" % volume)
density = psychrometrics.density(state, 101325, 24, 0.009)
print("Python API Test: Expected rho ~ 1.17 kg/m3 Calculated: %8.4f" % density)
wb = psychrometrics.wet_bulb(state, 24, 0.009, 101325)
print("Python API Test: Expected WB ~ 17 C Calculated: %8.4f" % wb)
dp = psychrometrics.dew_point(state, 0.009, 101325)
dp2 = psychrometrics.dew_point_b(state, 24, 17, 101325)
print("Python API Test: Expected DP ~ 13 C Calculated: %8.4f, %8.4f" % (dp, dp2))
vaporDensity = psychrometrics.vapor_density(state, 24, 0.009, 101325)
vaporDensity_2 = psychrometrics.vapor_density_b(state, 24, 0.5)
print("Python API Test: Expected VapDensity ~ 0.0107 kg/m3 Calculated: %8.4f, %8.4f" % (vaporDensity, vaporDensity_2))
cp = psychrometrics.specific_heat(state, 0.009)
print("Python API Test: Expected Cp ~ 1007 J/kgK Calculated: %8.4f" % cp)
energy = psychrometrics.latent_energy_of_air(state, 24)
print("Python API Test: Calculated energy?: %8.4f" % energy)
moisture_energy = psychrometrics.latent_energy_of_moisture_in_air(state, 24)
print("Python API Test: Calculated energy of moisture: %8.4f" % moisture_energy)

# check that we get error messages back:
error_count = 0


def error_handler(severity: int, message: bytes) -> None:
    global error_count
    error_count += 1


api.functional.callback_error(state, error_handler)
erroneous_dew_point = psychrometrics.dew_point_b(state, 16, 17, 101325)
print("Python API Test: Got back erroneous value of dew point: %8.4f\n" % erroneous_dew_point)
if error_count > 0:
    print("Python API Test: Errors were caught during dew point calculation, good!")
else:
    print("Python API Test: Errors were NOT caught during dew point calculation, bad!")
    sys.exit(1)
