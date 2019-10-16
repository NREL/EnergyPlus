Alternative Models for Clear Sky Emissivity Calculation
================

**Xuan Luo, Tianzhen Hong, Yukai Tomsovic**

**Lawrence Berkeley National Laboratory**

 - Original Date: October 15, 2019 

## Justification for New Feature ##

Sky emissivity, which represents the complex combination of the spectrally-dependent emissivity of atmospheric constituents, is fundamental to the calculation of downwelling longwave atmospheric radiation [1]. In EnergyPlus, the clear sky emissivity is used to calculated the clear sky temperature. Over the last century, researchers have proposed empirical sky emissivity models as a function of meteorological variables, including ambient temperature, water vapor pressure, or dew point temperature. The differences in these models result in large differences in estimations of thermal energy exchange that can be amplified by climate or application, such as radiative cooling [2].

Currently, EnergyPlus uses the Clark-Allen model (1978) based on a logarithmic relationship to dew point temperature, which was formulated using measurements limited to data collected over one year in San Antonio, Texas. While the original authors reported a low root mean square error (RMSE) of 10 W/m<sup>2</sup>, studies by the International Energy Agency and Dai and Fang demonstrated that, out of the empirical models examined, Clark-Allen had among the highest high errors when tested against MODTRAN predictions and observed data, respectively [3-4]. Further illustrating the limitations of Clark-Allen, a more recent assessment by Zhang et al shows that application of this model to calculate downwelling radiation in all-sky conditions tends to result in larger errors for low altitude and humid climates [5]. Literature indicates that this model is of insufficient accuracy. However, because the accuracy of emissivity models is limited by available measured data and strongly correlated to local conditions, it is difficult to establish a clear consensus among literature of the “best” model. For example, the most accurate model presented is often the model that was developed or fitted to the same region or climate as the dataset being studied. Despite this challenge, there is convincing evidence that more appropriate models than Clark-Allen for wide application to the contiguous United States exist. 

We propose to add the Brunt (1932) and Idso (1981) models, identified as two unique models with higher accuracy compared to Clark-Allen and other existing popular models. Researchers have studied both models extensively for a wide variety of climates and locations. This feature update would allow the user to select from these two models as alternatives to Clark-Allen to calculate the sky emissivity and temperature. 

## E-mail and  Conference Call Conclusions ##

N/A

## Overview ##

Brunt (1932) proposed the following relationship between water vapor pressure (P<sub>w</sub>, hPa) and clear emissivity [6]:

ε = c<sub>1</sub> + c<sub>2</sub>(P<sub>w</sub>)1/2  &nbsp;&nbsp;    Eq. (1)

Idso (1981) uses both water vapor pressure (Pw, hPa) and ambient temperature (T<sub>a</sub>, K) in this empirical model [7]

ε = c<sub>1</sub> + c<sub>2</sub> (P<sub>w</sub> ∙ exp(c<sub>3</sub>/T<sub>a</sub>))  &nbsp;&nbsp;    Eq. (2)

where c<sub>1</sub>, c<sub>2</sub>, and c<sub>3</sub> are empirical coefficients. Brunt (1932) and Idso (1981) are among the most widely accepted models, and both show improved performance over Clark-Allen in comparisons for locations across the United States [3-4]. When analyzed against observed longwave irradiance data, irradiation calculated with these models had relative RMSEs as low as 4% [8–10]. Table 1 summarizes studies that examined the performance of these two models in calculating longwave radiation (LW, W/m<sup>2</sup>), which is a function of sky emissivity and ambient temperature [11]: 

LW = ε ∙ 5.6697 ∙ 10-8 ∙ T<sub>a</sub><sup>4</sup>  &nbsp;&nbsp;    Eq (3)

In order to address the challenge of location dependence, some researchers fit the model to their own dataset and found significantly improved estimations. One such research team, Li et al, performed a grid-search recalibration using data from seven SURFRAD stations across the United States. Their study indicates that, after calibration, the models not only improved significantly in accuracy but also can be grouped into a few families yielding the same longwave irradiance values [10]. This suggests that one of the most important factors in the accuracy of the proposed model is the fitting dataset, for local climate and geography cause large variations in emissivity. Original formulations of these models using small datasets in one location to fit the relationship between the desired meteorological variable and sky emissivity are likely to produce large errors when applied to data outside that region [12]. Differences between the original and calibrated forms can be seen in Table 1. Newly calibrated forms of the Brunt (1932) and Idso (1981) models proposed by Li et al are chosen for implementation in EnergyPlus. Li et al’s extensive dataset of over 30,000 data points and subsequent analysis provides conclusive results that both of these models will accurately estimate sky emissivity. The calibrated forms of Brunt (1932) and Idso (1981) from Li et al as well as the Clark-Allen (1978) model are listed:

Clark-Allen (1978): ε = 0.787 + 0.764・ln(T<sub>d</sub>/T<sub>Kelvin</sub>)  &nbsp;&nbsp;    Eq. (4)

Brunt (1932), calibrated (2017): ε = 0.618 + 0.056(P<sub>w</sub>)1/2 	&nbsp;&nbsp;    Eq. (5)

Idso (1981), calibrated (2017): ε = 0.685 + 3.2 ∙ 10-5 ∙ P<sub>w</sub> ∙ exp(1699/T<sub>a</sub>)  &nbsp;&nbsp;    Eq. (6)

where T<sub>d</sub> is dewpoint temperature (K), P<sub>w</sub> is water vapor pressure (hPa), and T<sub>a</sub> is ambient temperature (K). 


**Table 1. Summary of reported clear sky atmospheric longwave radiation errors compared to observed data**

| Reference                        | Location                          | RMSE (W/m2) - Brunt (1932) | RMSE (W/m2) - Idso (1981) | RMSE (W/m2) - Clark-Allen (1978) | 
|----------------------------------|-----------------------------------|----------------------------|---------------------------|----------------------------------| 
| Li et al (2017)                  | 7 sites in US                     | 32.24                      | 14.03                     | -                                | 
| Li et al (2017), calibrated      | 7 sites in US                     | 13.24                      | 13.18                     | -                                | 
| Dai & Fang (2014)                | 7 sites in US                     | 22                         | -                         | 42                               | 
| Carmona et al (2014)             | Tandil, Argentina                 | 18                         | 30                        | -                                | 
| Carmona et al (2014), calibrated | Tandil, Argentina                 | 13                         | 13                        | -                                | 
| Flerchinger et al (2009)         | 21 sites in North America & China | 27                         | 18.5                      | -                                | 
| Choi et al (2008)                | Florida                           | 12.3                       | -                         | -                                | 

This improved sky emissivity feature would allow the user to select one of the three models, the calibrated Brunt (1932), the calibrated Idso (1981), and Clark-Allen (1978) using common meteorological inputs of ambient temperature (all), dewpoint temperature (Clark-Allen), and water vapor pressure (Idso and Brunt). The user can expect errors within 4-5% or around 14 W/m<sup>2</sup> when using Brunt (1932) or Idso (1981) as reported by Li et al. 

## Approach ##

We propose to add a new field to the existing WeatherProperty:SkyTemperature object to indicate the model to calculate the clear sky emissivity for calculating sky temperature.

```
WeatherProperty:SkyTemperature,
       \memo This object is used to define alternative sky temperature calculation or schedule import methods.
  A1,  \field Name
       \note blank in this field will apply to all run periods (that is, all objects=
       \note SizingPeriod:WeatherFileDays, SizingPeriod:WeatherFileConditionType or RunPeriod
       \note otherwise, this name must match one of the environment object names.
       \type object-list
       \object-list RunPeriodsAndDesignDays
  A2,  \field Calculation Type
       \required-field
       \note The field indicates that the sky temperature will be imported from external schedules or calculated by alternative methods other than default.
       \type choice
       \key ScheduleValue
       \key DifferenceScheduleDryBulbValue
       \key DifferenceScheduleDewPointValue
       \key UseBruntModel
       \key UseIdsoModel
  A3;  \field Schedule Name
       \note if name matches a SizingPeriod:DesignDay, put in a day schedule of this name
       \note if name is for a SizingPeriod:WeatherFileDays, SizingPeriod:WeatherFileConditionType or
       \note RunPeriod, put in a full year schedule that covers the appropriate days.
       \note Required if Calculation Type is ScheduleValue, DifferenceScheduleDryBulbValue or DifferenceScheduleDewPointValue.
       \type object-list
       \object-list DayScheduleNames
       \object-list ScheduleNames
```

## Testing/Validation/Data Source(s) ##

With the example file - DOE reference small office model, we will compare the sky emissivity and temperature calculation results between models.

## Input Output Reference Documentation ##

To be developed.

## Input Description ##

The `Calculation Type` field under the _WeatherProperty:SkyTemperature_ object will be modified to take two new keys as the choice:

- **UseBruntModel** 
- **UseIdsoModel** 

## Outputs Description ##

N/A

## Engineering Reference ##

To be developed.

## Example Files and Transition Changes ##

The existing DOE reference small office model will be modified to use alternative clear sky emissivity calculation methos.

No transition change is required.

## References ##

[1]	M. Cucumo, A. De Rosa, and V. Marinelli, “Experimental testing of correlations to calculate the atmospheric ‘transparency window’ emissivity coefficient,” Sol. Energy, vol. 80, no. 8, pp. 1031–1038, Aug. 2006.

[2]	L. Evangelisti, C. Guattari, and F. Asdrubali, “On the sky temperature models and their influence on buildings energy performance: A critical review,” Energy Build., vol. 183, pp. 607–625, 2019.

[3]	J. A. Olseth and A. Skartveit, “Review and Test of Parameterizations of Atmospheric Radiation,” 1994.

[4]	Q. Dai and X. Fang, “A new model for atmospheric radiation under clear sky condition at various altitudes,” Adv. Sp. Res., vol. 54, no. 6, pp. 1044–1048, Sep. 2014.

[5]	K. Zhang, T. P. Mcdowell, and M. Kummert, “Sky Temperature Estimation and Measurement for Longwave Radiation Calculation,” no. 3, pp. 769–778, 2017.

[6]	D. Brunt, “Notes on Radiation in the Atmosphere,” 1932.

[7]	S. B. Idso, “A set of equations for full spectrum and 8-14 um thermal radiation from cloudless skies,” Water Resour. Res., vol. 17, no. 2, pp. 295–304, 1981.

[8]	M. Choi, J. M. Jacobs, and W. P. Kustas, “Assessment of clear and cloudy sky parameterizations for daily downwelling longwave radiation over different land surfaces in Florida, USA,” Geophys. Res. Lett., vol. 35, no. 20, 2008.

[9]	F. Carmona, R. Rivas, and V. Caselles, “Estimation of daytime downward longwave radiation under clear and cloudy skies conditions over a sub-humid region,” Theor. Appl. Climatol., vol. 115, no. 1–2, pp. 281–295, 2014.

[10] M. Li, Y. Jiang, and C. F. M. Coimbra, “On the determination of atmospheric longwave irradiance under all-sky conditions,” Sol. Energy, vol. 144, pp. 40–48, 2017.

[11] U.S. DOE, “Engineering Reference,” 2019.

[12] M. G. G. Iziomon, H. Mayer, and A. Matzarakis, “Downward atmospheric longwave irradiance under clear and cloudy skies: Measurement and parameterization,” J. Atmos. Solar-Terrestrial Phys., vol. 65, no. 10, pp. 1107–1116, Jul. 2003.


