# Psychrometric services

EnergyPlus has a full complement of psychrometric functions. All the routines are Fortran functions returning a single precision real value.  All arguments and results are in SI units.

> Note that each of the psychrometric routines has a "calledfrom" optional parameter – this has been implemented in some of the calling routines and is useful when errors are detected during simulation for support personnel to figure out where the psych routine is called from.

The Names for the different Psychrometric Routines are based on the following self-explanatory format; the different variables used in the Psych Routine taxonomy are as follows.

H = Enthalpy

W= Humidity Ratio

Rh= Relative Humidity

V= Specific Volume

Rhov= Vapor Density of Air

Hfg = Latent energy (heat of vaporization for moist air)

Hg= Enthalpy of gaseous moisture

Pb= Barometric Pressure

Twb=Temperature Wet Bulb

Twd= Temperature Dry Bulb

Tdp= Temperature Dew Point

Tsat and Psat= Saturation Temperature and Saturation Pressure

Psy## Fn ##     = Psy {## is a Function of  ##}

Note: Each of the two capital alphabets together have different meaning

       Eg:     **{Psy ## Fn HW}= {Psy ## Function of  Enthalpy and Humidity Ratio}**

## PsyRhoAirFnPbTdbW (Pb,Tdb,W,calledfrom)

Returns the density of air in kilograms per cubic meter as a function of barometric pressure [Pb] (in Pascals), dry bulb temperature [Tdb] (in Celsius), and humidity ratio [W] (kilograms of water per kilogram of dry air).

## PsyCpAirFnWTdb (W,Tdb,calledfrom)

Returns the specific heat of air in Joules per kilogram degree Celsius as a function of humidity ratio [W] (kilograms of water per kilogram of dry air) and dry bulb temperature [Tdb] (Celsius).

## PsyHfgAirFnWTdb (W,Tdb,calledfrom)

Returns the Latent energy of air [Hfg](Joules per kilogram) as a function of humidity ratio [W] (kilograms of water per kilogram of dry air) and dry bulb temperature [Tdb] (Celsius).  It calculates hg and then hf and the difference is Hfg.

## PsyHgAirFnWTdb (W,Tdb,calledfrom)

Returns the specific enthalpy of the moisture as a gas in the air in Joules per kilogram as a function of humidity ratio [W] (kilograms of water per kilogram of dry air) and dry bulb temperature [Tdb] (Celsius).

## PsyTdpFnTdbTwbPb (Tdb,Twb,Pb,calledfrom)

Returns the dew point temperature in Celsius as a function of dry bulb temperature [Tdb] (Celsius), wet bulb temperature [Twb] (Celsius), and barometric pressure [Pb] (Pascals).

## PsyTdpFnWPb (W,Pb,calledfrom)

Returns the dew point temperature in Celsius as a function of humidity ratio [W] (kilograms of water per kilogram of dry air) and barometric pressure [Pb] (Pascals).

## PsyHFnTdbW (Tdb,W,calledfrom)

Returns the specific enthalpy of air in Joules per kilogram as a function of dry bulb temperature [Tdb] (Celsius) and humidity ratio [W] (kilograms of water per kilogram of dry air).

## PsyHFnTdbRhPb (Tdb,Rh,Pb,calledfrom)

Returns the specific enthalpy of air in Joules per kilogram as a function of dry bulb temperature [Tdb] (Celsius), relative humidity [Rh] (fraction), and barometric pressure [Pb] (Pascals).

## PsyTdbFnHW  (H,W,calledfrom)

Returns the air temperature in Celsius as a function of air specific enthalpy [H] (Joules per kilogram) and humidity ratio [W] (kilograms of water per kilogram of dry air).

## PsyRhovFnTdbRh (Tdb,Rh,calledfrom)

Returns the Vapor Density in air [RhoVapor](kilograms of water per cubic meter of air) as a function of dry bulb temperature [Tdb](Celcius), Relative Humidity [Rh] (fraction).

## PsyRhovFnTdbWP (Tdb,W,Pb,calledfrom)

Returns the Vapor Density in air [RhoVapor](kilograms of water per cubic meter of air) as a function of dry bulb temperature [Tdb](Celcius), humidity ratio [W] (kilograms of water per kilogram of dry air) and barometric pressure [Pb] (Pascals).

## PsyRhFnTdbRhov (Tdb,Rhov,calledfrom)

Returns the Relative Humidity [Rh] (fraction) in air as a function of dry bulb temperature [Tdb] (Celcius) and Vapor Density in air [RhoVapor](kilograms of water per cubic meter of air).

## PsyRhFnTdbWPb (Tdb,W,Pb,calledfrom)

Returns the relative humifity (fraction) as a function of of dry bulb temperature [Tdb] (Celsius), humidity ratio [W] (kilograms of water per kilogram of dry air) and barometric pressure [Pb] (Pascals).

## PsyTwbFnTdbWPb (Tdb,W,Pb,calledfrom)

Returns the air wet bulb temperatute in Celsius as a function of dry bulb temperature [Tdb] (Celsius), humidity ratio [W] (kilograms of water per kilogram of dry air) and barometric pressure [Pb] (Pascals).

## PsyVFnTdbWPb (Tdb,W,Pb,calledfrom)

Returns the specific volume in cubic meters per kilogram as a function of dry bulb temperature [Tdb] (Celsius), humidity ratio [W] (kilograms of water per kilogram of dry air) and barometric pressure [Pb] (Pascals).

## PsyWFnTdpPb (Tdp,Pb,calledfrom)

Returns the humidity ratio in kilograms of water per kilogram of dry air as a function of the dew point temperature [Tdp] (Celsius) and barometric pressure [Pb] (Pascals).

## PsyWFnTdbH (Tdb,H,calledfrom)

Returns the humidity ratio in kilograms of water per kilogram of dry air as a function of dry bulb temperature [Tdb] (Celsius) and air specific enthalpy [H] (Joules per kilogram).

## PsyWFnTdbTwbPb (Tdb,Twb,Pb,calledfrom)

Returns the humidity ratio in kilograms of water per kilogram of dry air as a function of dry bulb temperature [Tdb] (Celsius), wet bulb temperature [Twb] (Celsius), and barometric pressure [Pb] (Pascals).

## PsyWFnTdbRhPb (Tdb,Rh,Pb,calledfrom)

Returns the humidity ratio in kilograms of water per kilogram of dry air as a function of dry bulb temperature [Tdb] (Celsius), relative humidity [RH] (fraction), and barometric pressure [Pb] (Pascals).

## PsyPsatFnTemp (T,calledfrom)

Returns the saturation pressure in Pascals as a function of the air saturation temperature [T] (Celsius).

## PsyTsatFnHPb  (H,Pb,calledfrom)

Returns the air saturation temperature in Celsius as a function of air specific enthalpy [H] (Joules per kilogram) and barometric pressure [Pb] (Pascals).

## PsyTsatFnPb (P,calledfrom)

Returns the air saturation temperature in Celsius as a function of saturation pressure [P] (Pascals).

## CPCW (Temp,calledfrom)

Returns Specific heat capacity   (Joule/kilogram/kelvin) for chilled water as function of temperature [T] (Celsius).

## CPHW (Temp,calledfrom)

Returns Specific heat capacity (Joule/kilogram/kelvin) for hot water as function of temperature [T] (Celsius).

## CVHW (Temp,calledfrom)

Returns Specific heat capacity (Joule/kilogram/kelvin) for hot water at constant volume as function of temperature [T] (Celsius).

## RhoH2O (Temp,calledfrom)

Returns density of water (kg/m3) as function of Temperature [T] (Celsius).