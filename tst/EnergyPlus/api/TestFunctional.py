import sys

from pyenergyplus.api import EnergyPlusAPI

api = EnergyPlusAPI()

# GLYCOL TESTS
glycol = api.functional.glycol(u"water")
for t in [5.0, 15.0, 25.0]:
    cp = glycol.specific_heat(t)
    rho = glycol.density(t)
    k = glycol.conductivity(t)
    viscosity = glycol.viscosity(t)
    print("Python API Test: Calculated properties at T=%s: %f, %f, %f, %f" % (t, cp, rho, k, viscosity))

# REFRIGERANT TESTS
refrigerant = api.functional.refrigerant("steam")
temperature = 100.0
satPress = refrigerant.saturation_pressure(temperature)  # expecting about 101325 Pa
thisPress = 100000
satTemp = refrigerant.saturation_temperature(thisPress)  # expecting about 100 degC
print("Python API Test: Saturated Properties: At 100C, Psat=%8.4f; at 100000Pa, Tsat=%8.4f" % (satPress, satTemp))
satLiqDens = refrigerant.saturated_density(temperature, 0.0)  # // liq = 958 kg/m3
satLiqCp = refrigerant.saturated_specific_heat(temperature, 0.0)  # liq = 4,216 J/kgK
satLiqEnth = refrigerant.saturated_enthalpy(temperature, 0.0)
print("C API Test: Sat Liq at 100C: rho=%8.4f, Cp=%8.4f, h=%8.4f" % (satLiqDens, satLiqCp, satLiqEnth))
satVapDens = refrigerant.saturated_density(temperature, 1.0)  # vap = 1/1.6718 ~~ 0.59 kg/m3
satVapCp = refrigerant.saturated_specific_heat(temperature, 1.0)  # vap = 2,080 J/kgK
satVapEnth = refrigerant.saturated_enthalpy(temperature, 1.0)
print("C API Test: Sat Vap at 100C: rho=%8.4f, Cp=%8.4f, h=%8.4f" % (satVapDens, satVapCp, satVapEnth))
enthDifference = satVapEnth - satLiqEnth  # vap-liq = 2,675,570-419,170 ~ 2,256,400 J/kg

# PSYCHROMETRIC TESTS
psychrometrics = api.functional.psychrometrics()
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
db = psychrometrics.dry_bulb(48000, 0.009)
print("Python API Test: Expected DB ~ 24 C Calculated: %8.4f" % db)
rh = psychrometrics.relative_humidity(24, 0.0107)
rh2 = psychrometrics.relative_humidity_b(24, 0.009, 101325)
print("Python API Test: Expected RH ~ 0.5 Calculated: %8.4f, %8.4f" % (rh, rh2))
hr = psychrometrics.humidity_ratio(24, 48000)
hr2 = psychrometrics.humidity_ratio_b(13, 101325)
hr3 = psychrometrics.humidity_ratio_c(24, 0.5, 101325)
hr4 = psychrometrics.humidity_ratio_d(24, 17, 101325)
print("Python API Test: Expected HumRat ~ 0.009 Calculated: %8.4f, %8.4f, %8.4f, %8.4f" % (hr, hr2, hr3, hr4))
tSat = psychrometrics.saturation_temperature(48000, 101325)
print("Python API Test: Expected Tsat ~ 17 C Calculated: %8.4f" % tSat)
pSat = psychrometrics.saturation_pressure(24)
print("Python API Test: Expected Psat ~ 2985 Pa Calculated: %8.4f" % pSat)
h = psychrometrics.enthalpy(24, 0.009)
h2 = psychrometrics.enthalpy_b(24, 0.5, 101325)
print("Python API Test: Expected Enth ~ 0.48000 J/kg Calculated: %8.4f, %8.4f" % (h, h2))
volume = psychrometrics.specific_volume(24, 0.009, 101325)
print("Python API Test: Expected v ~ 0.855 m3/kg Calculated: %8.4f" % volume)
density = psychrometrics.density(101325, 24, 0.009)
print("Python API Test: Expected rho ~ 1.17 kg/m3 Calculated: %8.4f" % density)
wb = psychrometrics.wet_bulb(24, 0.009, 101325)
print("Python API Test: Expected WB ~ 17 C Calculated: %8.4f" % wb)
dp = psychrometrics.dew_point(0.009, 101325)
dp2 = psychrometrics.dew_point_b(24, 17, 101325)
print("Python API Test: Expected DP ~ 13 C Calculated: %8.4f, %8.4f" % (dp, dp2))
vaporDensity = psychrometrics.vapor_density(24, 0.009, 101325)
vaporDensity_2 = psychrometrics.vapor_density_b(24, 0.5)
print("Python API Test: Expected VapDensity ~ 0.0107 kg/m3 Calculated: %8.4f, %8.4f" % (vaporDensity, vaporDensity_2))
cp = psychrometrics.specific_heat(0.009)
print("Python API Test: Expected Cp ~ 1007 J/kgK Calculated: %8.4f" % cp)
energy = psychrometrics.latent_energy_of_air(24)
print("Python API Test: Calculated energy?: %8.4f" % energy)
moisture_energy = psychrometrics.latent_energy_of_moisture_in_air(24)
print("Python API Test: Calculated energy of moisture: %8.4f" % moisture_energy)

# check that we get error messages back:
error_count = 0


def error_handler(message: bytes) -> None:
    global error_count
    error_count += 1


api.functional.callback_error(error_handler)
erroneous_dew_point = psychrometrics.dew_point_b(16, 17, 101325)
print("Python API Test: Got back erroneous value of dew point: %8.4f\n" % erroneous_dew_point)
if error_count > 0:
    print("Python API Test: Errors were caught during dew point calculation, good!")
else:
    print("Python API Test: Errors were NOT caught during dew point calculation, bad!")
    sys.exit(1)
