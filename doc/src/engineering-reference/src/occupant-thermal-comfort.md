# Occupant Thermal Comfort

The integration of a sophisticated building thermal analysis tool with thermal comfort models allows one to perform an energy analysis on a zone and simultaneously determine if the environmental control strategy will be sufficient for the occupants to be thermally comfortable. This chapter is intended to provide background on thermal comfort, present an overview of state of the art thermal comfort models and present the mathematical models that have been incorporated into Energy Plus.

Thermal comfort modeling is controlled primarily by the People input object. This includes input for selecting the type of thermal comfort model that is desired by the user as well as parameters that serve as inputs to all of the thermal comfort models. This includes the activity level, the work efficiency, the air velocity, and the clothing insulation level for people within the space.  All four of these parameters can be scheduled. More information on the People input object can be found in the EnergyPlus Input/Output Reference. More information on how each of these parameters is used and the specific modeling equations for the thermal comfort models can be found below.

## Background on Thermal Comfort Models

Throughout the last few decades, researchers have been exploring the thermal, physiological and psychological response of people in their environment in order to develop mathematical models to predict these responses. Researchers have empirically debated building occupants' thermal responses to the combined thermal effect of the personal, environmental and physiological variables that influence the condition of thermal comfort.

There are two personal variables that influence the condition of thermal comfort: the thermal resistance of the clothing (I~cl~), and the metabolic rate (H/A~Du~). The thermal resistance of the clothing (I~cl~) is measured in units of "clo." The 1985 ASHRAE Handbook of Fundamentals (ASHRAE 1985) suggests multiplying the summation of the individual clothing items clo value by a factor of 0.82 for clothing ensembles.

The metabolic rate (H/A~Du~), is a measure of the internal heat production rate of an occupant (H) w/hr. in per unit of "Dubois" body surface area (A~Du~) in units of m^2^. The DuBois body surface area is given by :

![](media/image6297.png)\


Using this equation, an area of 1.8 m^2^ represents the surface area of an average person of weight 70 kg. and height 1.73 m (Fanger 1967). The metabolic rate is measured in mets, where 1 met = 58.2 W/m^2^.

The environmental variables that influence the conditions of thermal comfort include:

(1) Air Temperature (T~a~),

(2) Mean Radiant Temperature (T~r~),

(3) Relative air velocity (v),

(4) Water vapor pressure in ambient air (P~a~)

The Air Temperature (T~a~), a direct environmental index, is the dry-bulb temperature of the environment. The Mean Radiant Temperature (T~r~) is a rationally derived environmental index defined as the uniform black-body temperature that would result in the same radiant energy exchange as in the actual environment. The Relative air velocity (v) a direct environmental index is a measure of the air motion obtainable via a hot wire or vane anemometers. The Water vapor pressure in ambient air (P~a~) is a direct environmental index.

The physiological variables that influence the conditions of thermal comfort include:

(1) Skin Temperature (T~sk~),

(2) Core or Internal Temperature (T~cr~),

(3) Sweat Rate,

(4) Skin Wettedness (w),

(5) Thermal Conductance (K) between the core and skin.

Where the Skin Temperature (T~sk~), the Core Temperature (T~cr~) and the Sweat Rate are physiological indices. The Skin Wettedness (w) is a rationally derived physiological index defined as the ratio of the actual sweating rate to the maximum rate of sweating that would occur if the skin were completely wet.

One more consideration is important in dealing with thermal comfort - the effect of asymmetrical heating or cooling. This could occur when there is a draft or when there is a radiant flux incident on a person (which is what is of primary interest to us here). Fanger (1967) noted that the human regulatory system is quite tolerant of asymmetrical radiant flux. A reasonable upper limit on the difference in mean radiant temperature (T~r~) from one direction to the opposing direction is 15C. (ASHRAE 1984). This limit is lower if there is a high air velocity in the zone.

Table: General Nomenclature list for Thermal Comfort Models

Mathematical variable|Description|UnitsRange|FORTRAN variable
---------------------|-----------|----------|----------------
A~Du~|Dubois body surface area |m^2^|-|-
H|Internal heat production rate of an occupant per unit area |= M – W|W/m^2^|-|IntHeatProd
I~cl~|Thermal resistance of the clothing|clo|-|CloUnit
M|Metabolic rate per unit area|W/m^2^|-|ActLevel
P~a~|Water vapor pressure in ambient air |Torr|-|VapPress
T~a~|Air temperature|°C|-|AirTemp
T~cr~|Core or internal temperature|°C|-|CoreTemp
T~r~|Mean radiant temperature|°C|-|RadTemp
T~sk~|Skin temperature|°C|-|-
v|Relative air velocity|m/s|-|AirVel
W|The rate of heat loss due to the performance of work|W/m^2^|-|WorkEff
w|Skin wettedness|-|-|-

## Mathematical Models for Predicting Thermal Comfort

Many researchers have been exploring ways to predict the thermal sensation of people in their environment based on the personal, environmental and physiological variables that influence thermal comfort. From the research done, some mathematical models that simulate occupants' thermal response to their environment have been developed. Most thermal comfort prediction models use a seven or nine point thermal sensation scale, as in the following tables.

Table: Seven point Thermal Sensation Scale

Sensation|Description
---------|-----------
3|Hot
2|Warm
1|slightly warm
0|neutral
-1|slightly cool
-2|cool
-3|cold

Table: Nine point Thermal Sensation Scale

Sensation Value|Description
---------------|-----------
4|very hot
3|hot
2|warm
1|slightly warm
0|neutral
-1|slightly cool
-2|cool
-3|cold
-4|very cold

The most notable models have been developed by P.O. Fanger (the Fanger Comfort Model), the J. B. Pierce Foundation (the Pierce Two-Node Model), and researchers at Kansas State University (the KSU Two-Node Model). Berglund (1978) presents a detailed description of the theory behind these three models.

> Note for all Thermal Comfort reporting:  Though the published values for thermal comfort "vote" have a discrete scale (e.g. –3 to +3 or –4 to +4), the calculations in EnergyPlus are carried out on a continuous scale and, thus, reporting may be "off the scale" with specific conditions encountered in the space.  This is not necessarily an error in EnergyPlus – rather a different approach that does not take the "limits" of the discrete scale values into account.

The main similarity of the three models is that all three apply an energy balance to a person and use the energy exchange mechanisms along with experimentally derived physiological parameters to predict the thermal sensation and the physiological response of a person due to their environment. The models differ somewhat in the physiological models that represent the human passive system (heat transfer through and from the body) and the human control system (the neural control of shivering, sweating and skin blood flow). The models also differ in the criteria used to predict thermal sensation. However, all three models use information from the People statement and the thermal comfort model is selected via the People statement in a user's input file. Scheduled parameters such as the activity level, work efficiency, air velocity, and clothing insulation level all have a direct bearing on the thermal comfort models. For more information on the input of these parameters, see the People statement in the EnergyPlus Input/Output Reference. For more information on how each individual thermal comfort model uses these parameters, please consult the next several sections.

The main similarity of the three models is that all three apply an energy balance to a person and use the energy exchange mechanisms along with experimentally derived physiological parameters to predict the thermal sensation and the physiological response of a person due to their environment. The models differ somewhat in the physiological models that represent the human passive system (heat transfer through and from the body) and the human control system (the neural control of shivering, sweating and skin blood flow). The models also differ in the criteria used to predict thermal sensation.

## Fanger Comfort Model

Fanger's Comfort model was the first one developed. It was published first in 1967 (Fanger 1967) and then in 1970 (Fanger 1970), and helped set the stage for the other two models. The mathematical model developed by P.O. Fanger is probably the most well known of the three models and is the easiest to use because it has been put in both chart and graph form.

### Fanger Model Nomenclature List

Table: Nomenclature list for Fanger model

Mathematical variable|Description|Units|Range|FORTRAN variable
---------------------|-----------|-----|-----|----------------
A~Du~|Dubois body surface area |m^2^|-|BodySurfaceArea
C~res~|The rate of dry respiratory heat loss|W/m^2^|-|DryRespHeatLoss
E~dif~|The rate of heat loss from the diffusion of water vapor through the skin|W/m^2^|-|EvapHeatLossDiff
E~res~|The rate of latent respiratory heat loss|W/m^2^|-|LatRespHeatLoss
E~rsw,req~|The rate of heat loss from the evaporation of regulatory sweating at the state of comfort|W/m^2^|-|EvapHeatLossRegComf
E~sk~|Total evaporative heat loss from skin|W/m^2^||EvapHeatLoss
f~cl~|The ratio of clothed body|-||CloBodyRat
f~eff~|The fraction of surface effective for radiation |(= 0.72)|-|-|RadSurfEff
H|Internal heat production rate of an occupant per unit area (= M – W)|W/m^2^|-|IntHeatProd
h~c~|Convective heat transfer coefficient|W/m^2^°C|-|Hc
L|All the modes of energy loss from body |W/m^2^|-|-
M|Metabolic rate per unit area|W/m^2^|-|ActLevel
P~a~|Water vapor pressure in ambient air |Torr|-|VapPress
PMV|Predicted Mean Vote|-|-4~4|PMV
PPD|Predicted Percentage of Dissatisfied|-|0~100%|PPD
P~sk~|Saturated water vapor pressure at required skin temperature|Torr|-|SatSkinVapPress
Q~c~|The rate of convective heat loss|W/m^2^|-|ConvHeatLoss
Q~dry~|Sensible heat flow from skin|W/m^2^||DryHeatLoss
Q~r~|The rate of radiative heat loss|W/m^2^|-|RadHeatLoss
Q~res~|The rate of respiratory heat loss|W/m^2^|-|RespHeatLoss
T~a~|Air temperature|°C|-|AirTemp
T~cl~|Clothing surface temperature|°C|-|CloSurfTemp
T~cla~|Clothing surface temperature (Absolute)|°K|-|AbsCloSurfTemp
T~ra~|Mean radiant temperature|°K|-|AbsRadTemp
T~skr~|Skin temperature required to achieve thermal comfort|°C||SkinComfTemp
W|The rate of heat loss due to the performance of work|W/m^2^|-|WorkEff
|The emissivity of clothing-skin surface |-|-|SkinEmiss
|The Stefan-Boltzman constant (= 5.67×10^-8^)|W/m^2^K^4^|-|StefanBoltz

### Description of the model and algorithm

Fanger developed the model based on the research he performed at Kansas State University and the Technical University of Denmark. Fanger used the seven-point form of a thermal sensation scale along with numerous experiments involving human subjects in various environments. He related the subjects in response to the variables, which influence the condition of thermal comfort. Fanger's model is based upon an energy analysis that takes into account all the modes of energy loss (L) from the body, including: the convection and radiant heat loss from the outer surface of the clothing, the heat loss by water vapor diffusion through the skin, the heat loss by evaporation of sweat from the skin surface, the latent and dry respiration heat loss and the heat transfer from the skin to the outer surface of the clothing. The model assumes that the person is thermally at steady state with his environment.

![](media/image6298.png) W/m^2^

![](media/image6299.png) W/m^2^

![](media/image6300.png) W/m^2^

LatRespHeatLoss = 0.0023\*ActLevel\*(44. - VapPress)

DryRespHeatLoss = 0.0014\*ActLevel\*(34.- AirTemp)

RespHeatLoss = LatRespHeatLoss + DryRespHeatLoss

![](media/image6301.png) W/m^2^

![](media/image6302.png)  W/m^2^

![](media/image6303.png)  W/m^2^

ConvHeatLos = CloBodyRat\*Hc\*(CloSurfTemp - AirTemp)

RadHeatLoss = RadSurfEff\*CloBodyRat\*SkinEmiss\*StefanBoltz &

\*(AbsCloSurfTemp\*\*4 - AbsRadTemp\*\*4)

DryHeatLoss = ConvHeatLoss + RadHeatLoss

For![](media/image6304.png) , ![](media/image6305.png) W/m^2^

For ![](media/image6306.png) , ![](media/image6307.png) W/m^2^

![](media/image6308.png) W/m^2^

![](media/image6309.png)  W/m^2^

EvapHeatLossRegComf = 0.42\*(IntHeatProd - ActLevelConv)

EvapHeatLossRegComf = 0.0

EvapHeatLossDiff = 0.4148\*(SkinComfVpress - VapPress)

EvapHeatLoss = EvapHeatLossRegComf + EvapHeatLossDiff

Where,

0.68 is the passive water vapor diffusion rate, (g/h·m^2^·Torr)

0.61 is the latent heat of water, (W·h/g)

P~sk~ is the saturated water vapor pressure at the skin temperature required to achieve the thermal comfort

![](media/image6310.png) Torr

SatSkinVapPress = 1.92\*SkinTempComf - 25.3

![](media/image6311.png)  °C

SkinTempComf = 35.7 - 0.028\*IntHeatProd

By determining the skin temperature and evaporative sweat rate that a thermally comfortable person would have in a given set of conditions, the model calculates the energy loss (L). Then, using the thermal sensation votes from subjects at KSU and Denmark, a Predicted Mean Vote (PMV) thermal sensation scale is based on how the energy loss (L) deviates from the metabolic rate (M) in the following form:

![](media/image6312.png)\


ThermSensTransCoef = 0.303\*EXP(-0.036\*ActLevel) + 0.028

PMV = ThermSensTransCoef\*(IntHeatProd - EvapHeatLoss - RespHeatLoss - DryHeatLoss)

Predicted Percent of Dissatisfied (PPD) people at each PMV is calculated as follows:

PPD = 100.0 - 95.0\*EXP(-0.03353\*PMV\*\*4 - 0.2179\*PMV\*\*2)

## Pierce Two-Node Model

The Pierce Two-Node model was developed at the John B. Pierce Foundation at Yale University. The model has been continually expanding since its first publication in 1970 (Gagge et.al. 1970). The most recent version on the model appears in the 1986 ASHRAE Transactions (Gagge et.al. 1986).

### Pierce Two-Node Model Nomenclature List

Table: Nomenclature list for Pierce Two-Node model

Mathematical variable|Description|UnitsRange|FORTRAN variable
---------------------|-----------|----------|----------------
C~dil~|Constant for skin blood flow|||SkinBloodFlowConst
C~res~|The rate of dry respiratory heat loss|W/m^2^|-|DryRespHeatLoss
C~sw~|Proportionality constant for sweat control|g/m^2^hr||SweatContConst
DISC|Predicted discomfort vote |-|-5~5|DISC
E~dif~|The rate of heat loss from the diffusion of water vapor through the skin|W/m^2^|-|EvapHeatLossDiff
E~max~|Maximum evaporative heat loss|W/m^2^||EvapHeatLossMax
E~sk~|Total evaporative heat loss from skin|W/m^2^||EvapHeatLoss
E~res~|The rate of latent respiratory heat loss|W/m^2^|-|LatRespHeatLoss
E~rsw~|The rate of heat loss from the evaporation of regulatory sweating|W/m^2^|-|EvapHeatLossRegSweat
E~rsw,req~|The rate of heat loss from the evaporation of regulatory sweating at the state of comfort|W/m^2^||EvapHeatLossRegComf
ET\*|Effective Temperature|°C|-|ET
f~cl~|The ratio of clothed body|-||CloBodyRat
f~eff~|The fraction of surface effective for radiation |(= 0.72)|-|-|RadSurfEff
H|Internal heat production rate of an occupant per unit area (= M – W)|W/m^2^|-|IntHeatProd
h|Combined heat transfer coefficient|W/m^2^°C||H
h~c~|Convective heat transfer coefficient|W/m^2^°C|-|Hc
h~e~'|Combined evaporative heat transfer coefficient|W/(m^2^kP~a~)||-
h~r~|Radiant heat transfer coefficient|W/m^2^°C|-|Hr
I~cl~|Clothing insulation|m^2^°C/W||-
L|All the modes of energy loss from body |W/m^2^|-|-
L~ET\*~|All the modes of energy loss from body at ET\*|W/m^2^||-
L~SET\*~|All the modes of energy loss from body at SET\*|W/m^2^||-
M|Metabolic rate per unit area|W/m^2^|-|ActLevel
M~act~|Metabolic heat production due to activity|W/m^2^||-
M~shiv~|Metabolic heat production due to shivering|W/m^2^||ShivResponse
P~a~|Water vapor pressure in ambient air |Torr|-|VapPress
PMV\*|Predicted Mean Vote modified by ET\* or SET\*|-|-4~4|PMVET|PMVSET
P~SET\*~|Water vapor pressure at SET\*|°C||StdVapPressSET
P~sk~|Saturated water vapor pressure at required skin temperature|Torr|-|SatSkinVapPress
Q~c~|The rate of convective heat loss|W/m^2^|-|ConvHeatLoss
Q~crsk~|Heat flow from core to skin|W/m^2^||HeatFlow
Q~dry~|Sensible heat flow from skin|W/m^2^||DryHeatLoss
Q~r~|The rate of radiative heat loss|W/m^2^|-|RadHeatLoss
Q~res~|The rate of respiratory heat loss|W/m^2^|-|RespHeatLoss
S~cr~|Heat storage in core compartment|W/m^2^||CoreheatStorage
SET\*|Standard Effective Temperature|°C|-|SET
SIG~b~|Thermal signal of body|°C||BodyThermSigCold|BodyThermSigWarm
SIG~cr~|Thermal signal of core|°C||CoreThermSigCold|CoreThermSigWarm
SIG~sk~|Thermal signal of skin|°C||SkinThermSigCold|SkinThermSigWarm
SKBF|Skin blood flow|L/m^2^hr||SkinBloodFlow
S~sk~|Heat storage in skin compartment|W/m^2^||SkinHeatStorage
S~tr~|Constriction constant of skin blood flow for average person|||Str
SW~reg~|The rate of regulatory sweating|g/m^2^hr||RegSweat
T~a~|Air temperature|°C|-|AirTemp
T~b~|Mean body temperature|||AvgBodyTemp
T~b-c~|Mean body temperature when DISC is zero (lower limit)|°C||AvgBodyTempLow
T~b-h~|Mean body temperature when HSI is 100 (upper limit)|°C||AvgBodyTempHigh
T~cl~|Clothing surface temperature|°C|-|CloSurfTemp
T~cr~|Core or internal temperature|°C|-|CoreTemp
T~r~|Mean radiant temperature|°C|-|RadTemp
TSENS|Thermal sensation vote|-|-5~5|TSENS
T~sk~|Skin temperature|°C||SkinTemp
W|The rate of heat loss due to the performance of work|W/m^2^|-|WorkEff
w~dif~|Skin wettedness due to diffusion trough the skin|||SkinWetDiff
w~rsw~|Skin wettedness due to regulatory sweating|||SkinWetSweat
|The emissivity of clothing-skin surface |-|-|SkinEmiss
|The Stefan-Boltzman constant (= 5.67×10^-8^)|W/m^2^K^4^|-|StefanBoltz

### Description of the model and algorithm

The Pierce model thermally lumps the human body as two isothermal, concentric compartments, one representing the internal section or core (where all the metabolic heat is assumed to be generated and the skin comprising the other compartment). This allows the passive heat conduction from the core compartment to the skin to be accounted for. The boundary line between two compartments changes with respect to skin blood flow rate per unit skin surface area (SKBF in L/h•m^2^) and is described by alpha – the fraction of total body mass attributed to the skin compartment (Doherty and Arens 1988).

![](media/image6313.png)\


SkinMassRat = 0.0417737 + 0.7451832/(SkinBloodFlow + 0.585417)

Furthermore, the model takes into account the deviations of the core, skin, and mean body temperature weighted by alpha from their respective setpoints. Thermoregulatory effector mechanisms (Regulatory sweating, skin blood flow, and shivering) are defined in terms of thermal signals from the core, skin and body (Doherty and Arens 1988).

![](media/image6314.png) °C

![](media/image6315.png) °C

![](media/image6316.png) °C

SkinThermSigWarm = SkinTemp - SkinTempSet

SkinThermSigCold = SkinTempSet - SkinTemp

CoreThermSigWarm = CoreTemp - CoreTempSet

CoreThermSigCold = CoreTempSet - CoreTemp

BodyThermSigWarm = AvgBodyTemp - AvgBodyTempSet

BodyThermSigCold = AvgBodyTempSet-AvgBodyTemp

![](media/image6317.png) L/hr•m^2^

VasodilationFac = SkinBloodFlowConst\*CoreWarmDelTemp

VasoconstrictFac = Str\*SkinColdDelTemp

SkinBloodFlow = (6.3 + VasodilationFac)/(1. + VasoconstrictFac)

![](media/image6318.png)  g/hr•m^2^

RegSweat = SweatContConst\*BodyWarmDelTemp\*EXP(SkinWarmDelTemp/10.7)

![](media/image6319.png)  W/m^2^

ShivResponse = 19.4\*SkinThermSigCold\*CoreThermSigCold

The latest version of the Pierce model (Fountain and Huizenga 1997) discusses the concepts of SET\* and ET\*. The Pierce model converts the actual environment into a "standard environment" at a Standard Effective Temperature, SET\*. SET\* is the dry-bulb temperature of a hypothetical environment at 50% relative humidity for subjects wearing clothing that would be standard for the given activity in the real environment. Furthermore, in this standard environment, the same physiological strain, i.e. the same skin temperature and skin wettedness and heat loss to the environment, would exist as in the real environment. The Pierce model also converts the actual environment into a environment at an Effective Temperature, ET\*, that is the dry-bulb temperature of a hypothetical environment at 50% relative humidity and uniform temperature (Ta = MRT) where the subjects would experience the same physiological strain as in the real environment.

In the latest version of the model it is suggested that the classical Fanged PMV be modified by using ET\* or SET\* instead of the operative temperature. This gives a new index PMV\* which is proposed for dry or humid environments. It is also suggested that PMV\* is very responsive to the changes in vapor permeation efficiency of the occupants clothing.

![](media/image6320.png) W/m^2^

ActLevel = ActLevel + ActShiv

![](media/image6321.png) W/m^2^

![](media/image6322.png) W/m^2^

LatRespHeatLoss = 0.017251\*ActLevel\*(5.8662 - VapPress)

DryRespHeatLoss = 0.0014\*ActLevel\*(34.- AirTemp)

RespHeatLoss = LatRespHeatLoss + DryRespHeatLoss

![](media/image6323.png)  W/m^2^

![](media/image6324.png)  W/m^2^

![](media/image6325.png)  W/m^2^

DryHeatLoss = CloBodyRat\*(Hc\*(CloSurfTemp - AirTemp) + Hr\*(CloSurfTemp - RadTemp))

In Pierce model, the convective heat transfer coefficient, h~c~, varies with the air velocity around body and metabolic rate. The model uses the maximum value of following equations.

![](media/image6326.png)  W/m^2^°C

![](media/image6327.png)  W/m^2^°C

Hc = 8.6\*AirVel\*\*0.53

HcAct = 5.66\*(ActMet - 0.85)\*\*0.39

Also, in the model, the radiant heat transfer coefficient, h~r~, is defined by following equation (Doherty and Arens 1988):

![](media/image6328.png)  W/m^2^°C

Hr = 4.\*RadSurfEff\*StefanBoltz\*((CloSurfTemp + RadTemp)/2. + TAbsConv)\*\*3

In the Pierce model, T~cl~ is estimated by each iteration using following equation:

![](media/image6329.png) °C

CloSurfTemp = (CloCond\*SkinTemp + CloBodyRat\*(Hc\*AirTemp &

+ Hr\*RadTemp))/(CloCond + CloBodyRat\*(Hc + Hr))

Total evaporative heat loss from the skin, E~sk~, includes evaporation of water produced by regulatory sweating, E~rsw~, and evaporation of water vapor that diffuses through the skin surface, E~diff~.

![](media/image6330.png) W/m^2^

EvapHeatLoss = EvapHeatLossRegSweat + EvapHeatLossRegDiff

![](media/image6331.png)  W/m^2^

![](media/image6332.png)  W/m^2^

RegHeatLoss = 0.68\*RegSweat

DiffHeatLoss = SkinWetDiff\*MaxEvapHeatLoss

Where,

0.68 is the passive water vapor diffusion rate in g/h·m^2^·Torr

and,

![](media/image6333.png)\


![](media/image6334.png) W/m^2^

![](media/image6335.png)\


SkinWetDiff = (1.-SkinWetSweat)\*.06

MaxEvapHeatLoss = (1./TotEvapHeatResist)\*(SatSkinVapPress - VapPress)

SkinWetSweat = EvapHeatLossRegSweat/MaxEvapHeatLoss

The Pierce model has one additional heat flow term describing the heat transfer between the internal core compartment and the outer skin shell (Doherty and Arens 1988).

![](media/image6336.png) W/m^2^

HeatFlow = (CoreTemp-SkinTemp)\*(5.28 + 1.163\*SkinBloodFlow)

Where

5.28 is the average body tissue conductance in W/m^2^•°C

1.163 is the thermal capacity of blood in W•h/L•°C

Thus, individual heat balance equations for core and skin compartments are expressed using this term, Q~c-s~. New temperatures of core, skin and body are calculated by each iteration from rates of heat storage in the core and skin.

![](media/image6337.png) W/m^2^°C

SkinHeatStorage = HeatFlow - DryHeatLoss - EvapHeatLoss

![](media/image6338.png)  W/m^2^°C

CoreHeatStorage = IntHeatProd - RespHeatLoss  - HeatFlow

Thus,

![](media/image6339.png)\


![](media/image6340.png)\


ThermSensTransCoef = 0.303\*EXP(-0.036\*ActLevel) + 0.028

PMVET = ThermSensTransCoef\*(IntHeatProd - EvapHeatLossDiff  &

- EvapHeatLossRegComf - RespHeatLoss - DryHeatLossET)

PMVSET = ThermSensTransCoef\*(IntHeatProd - EvapHeatLossDiff  &

- EvapRegHeatLossReg Comf - RespHeatLoss - DryHeatLossSET)

Besides PMV\*, the Pierce Two Node Model uses the indices TSENS and DISC as predictors of thermal comfort. Where TSENS is the classical index used by the Pierce foundation, and is a function of the mean body temperature. DISC is defined as the relative thermoregulatory strain that is needed to bring about a state of comfort and thermal equilibrium. DISC is a function of the heat stress and heat strain in hot environments and equal to TSENS in cold environments. In summary, the Pierce Model, for our purposes, uses four thermal comfort indices; PMVET-a function of ET\*, PMVSET- a function of SET\*, TSENS and DISC.

![](media/image6341.png) °C

![](media/image6342.png) °C

![](media/image6343.png) ![](media/image6344.png)

![](media/image6345.png) ![](media/image6346.png)

![](media/image6347.png)\


AvgBodyTempLow = (0.185/ActLevelConv)\*IntHeatProd + 36.313

AvgBodyTempHigh = (0.359/ActLevelConv)\*IntHeatProd + 36.664

TSENS = .68175\*(AvgBodyTemp-AvgBodyTempLow)

TSENS = 4.7\*(AvgBodyTemp - AvgBodyTempLow)/ &

(AvgBodyTempHigh - AvgBodyTempLow)

DISC = 5.\*(EvapHeatLossRegSweat - EvapHeatLossRegComf)/ &

(MaxEvapHeatLoss - EvapHeatLossRegComf - DiffHeatLoss)

## KSU Two-Node Model

The KSU two-node model, developed at Kansas State University, was published in 1977 (Azer and Hsu 1977). The KSU model is quite similar to that of the Pierce Foundation. The main difference between the two models is that the KSU model predicts thermal sensation (TSV) differently for warm and cold environment.

### KSU Two Node Model Nomenclature List

Table: Nomenclature list for KSU Two-Node model

Mathematical variable|Description|UnitsRange|FORTRAN variable
---------------------|-----------|----------|----------------
C~cr~|Specific heat of body core|Whr/kg°C||
-----|--------------------------|--------||
C~sk~|Specific heat of skin|Whr/kg°C||
-----|---------------------|--------||
C~res~|The rate of dry respiratory heat loss|W/m^2^|-|DryRespHeatLoss
E~dif~|The rate of heat loss from the diffusion of water vapor through the skin|W/m^2^|-|EvapHeatLossDiff
E~max~|Maximum evaporative heat loss|W/m^2^||EvapHeatLossMax
E~sk~|Total evaporative heat loss from skin|W/m^2^||EvapHeatLoss
E~sw~|Equivalent evaporation heat loss from the sweat secreted|W/m^2^||EvapHeatLossSweat
E~sw.d~|Sweat function for warm and dry skin|W/m^2^||DrySweatRate
E~res~|The rate of latent respiratory heat loss|W/m^2^|-|LatRespHeatLoss
F~cl~|The Burton thermal efficiency factor for clothing||-|CloThermEff
F~pcl~|Permeation efficiency factor for clothing||-|CloPermeatEff
H|Internal heat production rate of an occupant per unit area |= M - W|W/m^2^|-|IntHeatProd
H|Combined heat transfer coefficient|W/m^2^°C||H
h~c~|Convective heat transfer coefficient|W/m^2^°C|-|Hc
h~r~|Radiant heat transfer coefficient|W/m^2^°C|-|Hr
KS|Overall skin thermal conductance|W/m^2^°C||ThermCndct
KS~o~|Skin conductance at thermal neutrality|W/m^2^°C||ThermCndctNeut
KS~(-4)~|Skin conductance at thermal sensation very cold|W/m^2^°C||ThermCndctMin
M|Metabolic rate per unit area|W/m^2^|-|ActLevel
M~shiv~|Metabolic heat production due to shivering|W/m^2^||ShivResponse
P~a~|Water vapor pressure in ambient air |Torr|-|VapPress
P~sk~|Saturated water vapor pressure at required skin temperature|Torr|-|SatSkinVapPress
PT~accl~|The pattern of acclimation|||AcclPattern
Q~c~|The rate of convective heat loss|W/m^2^|-|ConvHeatLoss
Q~dry~|Sensible heat flow from skin|W/m^2^||DryHeatLoss
Q~r~|The rate of radiative heat loss|W/m^2^|-|RadHeatLoss
Q~res~|The rate of respiratory heat loss|W/m^2^|-|RespHeatLoss
RH|Relative humidity|||RelHum
T~a~|Air temperature|°C|-|AirTemp
T~cr~|Core or internal temperature|°C|-|CoreTemp
T~o~|Operative temperature|°C|-|OpTemp
T~r~|Mean radiant temperature|°C|-|RadTemp
T~sk~|Skin temperature|°C||SkinTemp
TSV|Thermal sensation vote||-4~4|TSV
V|Relative air velocity|m/s|-|AirVel
W|The rate of heat loss due to the performance of work|W/m^2^|-|WorkEff
W|Skin wettedness|-|-|SkinWet
W~cr~|Mass of body core per unit body surface|kg/m^2^||-
-----|---------------------------------------|-------||-
w~rsw~|Skin wettedness due to regulatory sweating|||SkinWetSweat
w~rsw-o~|Skin wettedness at thermal neutrality|||SkinWetSweatNeut
W~sk~|Mass of skin per unit body surface|kg/m^2^||-
-----|----------------------------------|-------||-

### Description of the model and algorithm

The KSU two-node model is based on the changes that occur in the thermal conductance between the core and the skin temperature in cold environments, and in warm environments it is based on changes in the skin wettedness.

In this model metabolic heat production is generated in the core which exchanges energy with the environment by respiration and the skin exchanges energy by convection and radiation. In addition, body heat is dissipated through evaporation of sweat and/or water vapor diffusion through the skin. These principles are used in following passive system equations.

![](media/image6348.png) W/m^2^

![](media/image6349.png) W/m^2^

Where

![](media/image6350.png) W/m^2^

LatRespHeatLoss = 0.0023\*ActLevelTot\*(44. - VapPress)

DryRespHeatLoss = 0.0014\*ActLevelTot\*(34. - AirTemp)

RespHeatLoss = LatRespHeatLoss + DryRespHeatLoss

![](media/image6351.png) W/m^2^

DryHeatLoss = H\*CloBodyRat\*CloThermEff\*(SkinTemp - OpTemp)

![](media/image6352.png) W/m^2^°C

![](media/image6353.png) W/m^2^°C

![](media/image6354.png) W/m^2^°C

H = Hc + Hr

Hc = 8.3\*SQRT(AirVel)

Hr = 3.87 + 0.031\*RadTemp

![](media/image6355.png) °C

OpTemp = (Hc\*AirTemp + Hr\*RadTemp)/H

and

For ![](media/image6356.png) , ![](media/image6357.png) W/m^2^

For ![](media/image6358.png) , ![](media/image6359.png) W/m^2^

![](media/image6360.png) W/m^2^

![](media/image6361.png) W/m^2^

EvapHeatLoss = SkinWetSweat\*EvapHeatLossMax+(1. - SkinWetSweat)\*EvapHeatLossDiff

SkinWetSweat = EvapHeatLossDrySweat/EvapHeatLossMax

EvapHeatLossDiff = 0.408\*(SkinVapPress - VapPress)

EvapHeatLossMax = 2.2\*Hc\*(SkinVapPress - VapPress)\*CloPermeatEff

Here, control signals, based on setpoint temperatures in the skin and core, are introduced into passive system equations and these equations are integrated numerically for small time increments or small increments in core and skin temperature. The control signals modulate the thermoregulatory mechanism and regulate the peripheral blood flow, the sweat rate, and the increase of metabolic heat by active muscle shivering. The development of the controlling functions of skin conductance (KS), sweat rate (E~sw~), and shivering (M~shiv~) is based on their correlation with the deviations in skin and core temperatures from their setpoints.

![](media/image6362.png)\


SkinCndctDilation = 42.45\*CoreSignalWarmMax &

+ 8.15\*CoreSignalSkinSens\*\*0.8\*SkinSignalWarmMax

SkinCndctConstriction = 1.0 + 0.4\*SkinSignalColdMax

ThermCndct = 5.3+(6.75+SkinCndctDilation)/SkinCndctConstriction

![](media/image6363.png)\


WeighFac = 260.+70.\*AcclPattern

SweatCtrlFac = 1. + 0.05\*SkinSignalSweatColdMax\*\*2.4

DrySweatRate = ((WeighFac\*CoreSignalSweatMax &

+ 0.1\*WeighFac\*SkinSignalSweatMax) &

\*EXP(SkinSignalSweatMax/8.5))/SweatCtrlFac

Where

![](media/image6364.png) ![](media/image6365.png)

![](media/image6366.png) ![](media/image6367.png)

SweatSuppFac = 1.

SweatSuppFac = 0.5 + 0.5\*EXP(-5.6\*SkinWetSignal)

![](media/image6368.png) W/m^2^

ShivResponse = 20.\*CoreSignalShivMax\*SkinSignalShivMax + 5.\*SkinSignalShivMax

In KSU model, two new parameters are introduced and used in correlating thermal sensations with their associated physiological responses. In stead of correlating warm thermal sensations with skin wettedness, it is here correlated with a wettedness factor defined by

![](media/image6369.png)\


SkinWetFac = (SkinWetSweat - SkinWetNeut)/(1. - SkinWetNeut)

Where

![](media/image6370.png)\


![](media/image6371.png)\


SkinWetSweat = DrySweatRate/EvapHeatLossMax

SkinWetNeut = 0.02 + 0.4\*(1.-EXP(-0.6\*(IntHeatProdMetMax - 1.)))

and instead of correlating cold thermal sensation with the skin temperature, it is here correlated with a factor identified as vasoconstriction factor defined by

![](media/image6372.png)\


VasoconstrictFac = (ThermCndctNeut - ThermCndct) &

/(ThermCndctNeut - ThermCndctMin)

Thus, TSV in the cold is a function of a vasoconstriction factor (ε~vc~) as:

![](media/image6373.png)\


TSV = -1.46153\*VasoconstrictFac + 3.74721\*VasoconstrictFac\*\*2 &

- 6.168856\*VasoconstrictFac\*\*3

and for the warm environments, TSV is defined as:

![](media/image6374.png)\


TSV = (5. - 6.56\*(RelHum - 0.50))\*SkinWetFac

The KSU model's TSV was developed from experimental conditions in all temperature ranges and from clo levels between .05 clo to 0.7 clo and from activities levels of 1 to 6 mets (Berglund 1978).

**Adaptive Comfort Model**

Adaptive comfort model, intended for use in naturally ventilated buildings, determines the acceptability of indoor conditions given the monthly mean outdoor air temperature and the indoor operative temperature. This is used as an index for occupant adaptation to outdoor conditions, and determines the acceptability of indoor conditions. The model also accounts for people's clothing adaptation in naturally conditioned spaces by relating the acceptable range of indoor temperatures to the outdoor climate, so it is not necessary to estimate the clothing values for the space. No humidity or air-speed limits are required when this option is used. This section summarizes the adaptive comfort models based on the ASHRAE Standard 55-2010 and CEN 15251. Details are available in the two standards.

**Adaptive Comfort Model Based on ASHRAE Standard 55-2010**

In ASHRAE Standard 55, the monthly mean outdoor air temperature, used in the adaptive comfort model, is defined as the simple running average of the previous thirty daily average outdoor air temperatures.

The model defines two comfort regions: 80% Acceptability, and 90% Acceptability. If the monthly mean outdoor air temperature is not within the specified domain, the model is not applicable.

![Acceptable operative temperature ranges for naturally conditioned spaces (ASHRAE Standard 55-2010)](media/acceptable-operative-temperature-ranges-for.jpg)


The central line of the model (shown in red), or comfort temperature, is defined as

![](media/image6376.png)\


Where

T~o~~t~ – operative temperature (°C), calculated as the average of the indoor air dry-bulb temperature and the mean radiant temperature of zone inside surfaces

T~o~ – monthly mean outdoor air dry-bulb temperature (°C).

If the .stat file is provided for the simulation, T~o~ is drawn directly from the daily average temperatures in the .stat file, which provides a value for each month.  If no .stat file is provided, the monthly mean outdoor temperature is a simple running average of the previous thirty daily average temperatures, calculated directly from the weather file (.epw):

![](media/image6377.png)\


![](media/image6378.png)\


![](media/image6379.png)\


T~o~~d-i~is defined as the daily average temperature of the i^th^ previous day.

Note that the weather file must be a standard .epw containing a full year of data.

The comfort regions for 80% and 90% acceptability are symmetric about the central line.

90% Acceptability Limits: T~o~~t~ = 0.31\* T~o~ + 17.8 ± 2.5

80% Acceptability Limits: T~o~~t~ = 0.31\* T~o~ + 17.8 ± 3.5

If, using either method, T~o~~~is less than 10°(C) or greater than 33.5°(C), the model is not applicable.

For a detailed description of this model, please see *ASHRAE Standard 55-2010, Thermal Environmental Conditions for Human Occupancy*.

## Adaptive Comfort Model Based on European Standard EN15251-2007

The EN15251-2007 is similar to ASHRAE 55-2010, but with slightly different curves of the indoor operative temperature and acceptability limits (Fig. 2). The model, intended for use in naturally ventilated buildings, determines the acceptability of indoor conditions given the 7-day weighted mean outdoor air temperature and the indoor operative temperature. The 7-day weighted mean outdoor air temperature (T~r~~m~) is defined as the weighted running average of the previous 7 daily average outdoor air temperatures.

This weighted running average is calculated from a full annual weather file that must be specified for the simulation. This is used as an index for occupant adaptation to outdoor conditions, and determines the acceptability of indoor conditions. The model also accounts for people's clothing adaptation in naturally conditioned spaces by relating the acceptable range of indoor temperatures to the outdoor climate, so it is not necessary to estimate the clothing values for the space. No humidity or air-speed limits are required when this option is used. The model defines three comfort regions: Category I (90%) Acceptability, Category II (80%) Acceptability, and Category III (65%) Acceptability. If T~rm~ is not within the specified domain, the model is not applicable.

![Categories for European Standard EN15251-2007](media/categories-for-european-standard-en15251-2007.png)


![Acceptable operative temperature ranges for naturally conditioned spaces (CEN EN15251-2007)](media/acceptable-operative-temperature-ranges-for-001.jpg)


Central line (shown as red Figure 304): Tot = 0.33\*To + 18.8

Category I, 90% Acceptability Limits: Tot = 0.33\*To + 18.8 ± 2.0

Category II, 80% Acceptability Limits: Tot = 0.33\*To + 18.8 ± 3.0

Category III, 65% Acceptability Limits: Tot = 0.33\*To + 18.8 ± 4.0

For 10°(C) < T~rm~~~< 15°(C), the comfort temperature of the lower boundaries of the comfort regions is T~comf~ = 23.75°(C). That is, the lower boundaries are constant according to the same ranges above:

Category I, 90% Acceptability Limits: Tot = 23.75- 2.0

Category II, 80% Acceptability Limits: Tot = 23.75 - 3.0

Category III, 65% Acceptability Limits: Tot = 23.75 - 4.0

Where

Tot – operative temperature (°C) , calculated as the average of the indoor air dry-bulb temperature and the mean radiant temperature of zone inside surfaces

To – mean outdoor air dry-bulb temperature (°C), calculated as the weighted mean of the previous 7-day daily mean outdoor air dry-bulb temperature (T~od~):

T~o~ = (1 - α)\*{T~od-1~ + α \*T~o~~d~~-~~2~ + α^2^ \*T~o~~d~~-~~3~ + α^3^ \*T~o~~d~~-~~4~ + α^4^ \*T~o~~d~~-~~5~ + α^5^ \*T~o~~d~~-~~6~ + α^6^ \*T~o~~d~~-~~7~}

T~o~ = (1 - α)\*T~od-1~ + α \*T~o-1~

α  = 0.8

## Dynamic Clothing Model

In most building energy simulations, thermal comfort condition is calculated based on the assumption that the clothing insulation is equal to a constant value of 0.5 clo during the cooling season and 1.0 clo during heating season. Usually those two values are used and the change from 0.5 to 1 or vice-versa is made suddenly from one day to another. In addition, there is no standardized guideline on how to set clothing insulation schedules in the international standards. This simplified assumption may lead to systems that are incorrectly sized and operated and to the incorrect assessment of comfort conditions. In reality, occupants frequently adjust their clothing depending on the thermal conditions around them, as opposed to the assumption of constant clothing values. Therefore, the clothing insulation variation should be captured during the building simulation to realistically model HVAC systems. In order to overcome the limitations of the constant clothing insulation assumption, three new predictive clothing insulation models were developed by Schiavon and Lee (2012) based on 6,333 selected observations taken from ASHRAE RP-884 and RP-921 databases. The first and third models vary the clothing insulation as a function of outdoor air temperature measured at 6 o'clock and the second model takes into account both 6 o'clock outdoor air temperature and indoor operative temperature when adjusting the clothing insulation. The dynamic clothing models should be implemented in dynamic building energy simulation.

The model proposed to ASHRAE 55 is described below.

For t~a(out,6)~< -5°C, ~I~~cl~=1.00

For -5°C ≤ t~a(out,6)~< 5°C, ~I~~cl~=0.818-0.0364\*ta~(~~out~~,6)~

For 5°C ≤ t~a(out,6)~< 26°C ~I~~cl~=10(-0.1635-0.0066\*ta~(~~out~~,6)~)

or t~a(out,6)~≥ 26°C ~I~~cl~=0.46

Where, *I~cl~* is the clothing insulation value, *t~a(out, 6)~* is the outdoor air temperature measured at 6 o'clock in the morning. The following figure illustrates the proposed clothing insulation model.

![Clothing Schedule Illustration](media/clothing-schedule-illustration.png)


In the figure above, clothing insulation schedule for a fixed model (blue) typically used in energy simulation software and for the clothing model based on outdoor air temperature measured at 6 o'clock. Climate data for Chicago O'hare International Airport has been used.

The following figure illustrates the new clothing insulation model.

![Graphical representation of the proposed clothing insulation model](media/graphical-representation-of-the-proposed.png)


The dynamic predictive clothing insulation model is implemented into EnergyPlus for realistic energy simulation. Addenda A to ASHRAE 55 with the clothing model has been approved by the ASHRAE committee and the chance to be included in ASHRAE 55-2013 is high.

### References

Schiavon S, Lee KH. 2013. Dynamic predictive clothing insulation models based on outdoor air and indoor operative temperatures. Building and Environment. Volume 59, 250-260. http://dx.doi.org/10.1016/j.buildenv.2012.08.024  (link to the journal) http://escholarship.org/uc/item/3338m9qf  (link to the freely available pre-print version)

Lee KH, Schiavon S. 2013. Influence of three dynamic predictive clothing insulation models on building energy use, HVAC sizing and thermal comfort. Submitted to Journal. http://escholarship.org/uc/item/3sx6n876  (link to the freely available pre-print version)

## Mean Radiant Temperature Calculation

There are three options to calculate mean radiant temperature in the thermal comfort models. One is the zone averaged MRT, another is the surface weighted MRT, and the other is angle factor MRT. The zone averaged MRT is calculated on the assumption that a person is in the center of a space, whereas the surface weighted MRT is calculated in consideration of the surface that a person is closest to, and the angle factor MRT is calculated based on angle factors between a person and the different surfaces in a space. Here, the surface weighted MRT is the average temperature of the selected surface and zone averaged MRT and is intended to represent conditions in the limit as a person gets closer and closer to a particular surface.  In that limit, half of the person's radiant field will be dominated by that surface and the other half will be exposed to the rest of the zone.  Note that the surface weighted MRT is only an approximation. The angle factor MRT is the mean temperature of the surrounding surface temperatures weighted according to the magnitude of the respective angle factors and allows the user to more accurately predict thermal comfort at a particular location within a space.

Table: Nomenclature and variable list for MRT calculation

Mathematical variable|Description|UnitsRange|FORTRAN variable
---------------------|-----------|----------|----------------
T~r~|Mean radiant temperature|°C|-|RadTemp
T~r-avg~|Zone averaged radiant temperature|°C|-|ZoneRadTemp
T~surf~|Surface temperature|°C|-|SurfaceTemp
F~surf~|Angle factor between person and surface|-|0~1|AngleFactor

### Description of the model and algorithm

The zone averaged MRT is calculated without weighting any surface temperature of the space.

![](media/image6384.png)\


RadTemp = MRT(ZoneNum)

The surface weighted MRT is the average temperature of the zone averaged MRT and the temperature of the surface that a person is closest to.

![](media/image6385.png)\


ZoneRadTemp = MRT(ZoneNum)

SurfaceTemp = GetSurfaceTemp(People(PeopleNum)%SurfacePtr)

RadTemp = (ZoneRadTemp + SurfaceTemp)/2.0

The angle factor MRT is the mean value of surrounding surface temperatures weighted by the size of the respective angle factors between a person and each surface.

![](media/image6386.png)\


SurfTempAngleFacSummed = SurfTempAngleFacSummed &

+ SurfaceTemp \* AngleFactorList(AngleFacNum)%AngleFactor(SurfNum)

RadTemp = SurfTempAngleFacSummed

## References

ASHRAE. 1984. "High Intensity Infrared Radiant Heating", 1984 Handbook of Systems and Equipment, American Society of Heating, Refrigerating and Air Conditioning Engineers, Atlanta, GA, Chapter 18.

ASHRAE. 1985. "Physiological Principles for Comfort and Health," 1985 Handbook of Fundamentals, American Society of Heating, Refrigerating and Air Conditioning Engineers, Atlanta, GA, Chapter 8.

ASHRAE. 1993. "Physiological Principles and Thermal Comfort", 1993 ASHRAE Handbook of Fundamentals, American Society of Heating, Refrigerating and Air Conditioning Engineers, Atlanta, GA, Chapter 8.

ASHRAE. 2010. "Standard 55-2010 -- Thermal Environmental Conditions for Human Occupancy (ANSI approved)", American Society of Heating, Refrigerating and Air Conditioning Engineers, Atlanta, GA.

Azer, N.Z., Hsu, S. 1977. "The prediction of Thermal Sensation from Simple model of Human Physiological Regulatory Response", ASHRAE Trans., Vol.83, Pt 1.

Berglund, Larry. 1978. "Mathematical Models for Predicting the Thermal Comfort Response of Building Occupants", ASHRAE Trans., Vol.84.

Doherty, T.J., Arens, E. 1988. "Evaluation of the Physiological Bases of Thermal Comfort Models", ASHRAE Trans., Vol.94, Pt 1.

Du Bois, D. and E.F. 1916. "A Formula to Estimate Approximate Surface Area, if Height and Weight are Known", Archives of internal Medicine, Vol.17.

CEN. 2007. "Standard EN15251 Indoor environmental input parameters for design and assessment of energy performance of buildings addressing indoor air quality, thermal environment, lighting and acoustics". Bruxelles: European committee for Standardisation.

Fanger, P.O. 1970. Thermal Comfort-Analysis and Applications in Environmental Engineering, Danish Technical Press, Copenhagen.

Fanger, P.O. 1986. "Radiation and Discomfort", ASHRAE Journal. February 1986.

Fanger P.O. 1967. "Calculation of Thermal Comfort: Introduction of a Basic Comfort Equation", ASHRE Trans., Vol.73, Pt 2.

Fountain, Marc.E., Huizenga, Charlie. 1997 "A Thermal Sensation Prediction Tool for Use by the Profession", ASHRAE Trans., Vol.103, Pt 2.

Gagge, A.P., Stolwijk, J. A. J., Nishi, Y. 1970. "An Effective Temperature Scale Based on a Simple Model of Human Physiological Regulatory Response", ASHRAE Trans., Vol.70, Pt 1.

Gagge, A.P., Fobelets, A.P., Berglund, L. G. 1986. "A Standard Predictive Index of Human Response to the Thermal Environment",  ASHRAE Trans., Vol.92, Pt 2.

Hsu, S. 1977. "A Thermoregulatory Model for Heat Acclimation and Some of its Application", Ph. D. Dissertation, Kansas State University.

Int-Hout, D. 1990. "Thermal Comfort Calculation / A Computer Model", ASHRAE Trans., Vol.96, Pt 1.

ISO. 1983. "Determination of the PMV and PPD Indices and Specification of the Conditions for Thermal Comfort", DIS 7730, Moderate Thermal Environment, 1983.