# Complex Fenestration Daylighting Calculations

The complex fenestration systems (CFS) daylighting calculation method results in the same Daylighting Factor style output as the it is described in Daylighting Calculations chapter.

Transmitted solar radiation reaches the daylighting sensor in two different ways. The first is an indirect path when solar radiation is reflected from surfaces and uniformly distributed into the space. The second is a direct path when the sensor is directly illuminated by the fenestration system.

For the indirect path, the method performs a one-bounce calculation to determine the diffuse illumination for the zone. For a certain sky condition, including the sun position, the method will calculate which surfaces are illuminated from each of the BSDF directions, taking into account the reflectance of the surfaces. This indirect illumination is calculated from both direct sunlight hitting a window with or without a shading device and diffuse illumination calculated from diffuse light coming into the zone from the sky or the ground. This transmitted light strikes the zone interior surfaces and creates a diffuse component. The CFS daylighting method determines the contribution of each sky patch to the illumination of the zone surfaces based on the BSDF transmittances and then luminance is calculated based on the surface reflection.

For the direct path, the window is subdivided using existing subdivision procedures. From the center of each subdivided element, the angle is calculated to the reference point. The illuminance is calculated based on the BSDF data for the outgoing direction that corresponds to that angle and the incident illumination from sky, ground and sun.

## Internal Average Reflected Illuminance From Window

To calculate internal average reflected illumiance from the window it is necessary to calculate transmitted flux at the window.  Observing an infinitesimal window element, illuminance can originate from the sky or sun, and in both cases the ray can reach the window directly or after reflecting from the ground.  Based on this, the calculation of luminance at the window is divided into three parts:

Sky-related illuminance and luminous flux

Sun-related illuminance and luminous flux, excluding entering beam

Sun-related illuminance and luminous flux, due to entering beam

where total illuminance will be calculated as a superposition of these three parts.

For any case, illuminace at a window element can be calculated by using the following general approach:

![](media/image804.png)\


where ![](media/image805.png)  is the illuminance at a window element, ![](media/image806.png)  represents the function of horizontal luminance from the exterior element in the direction ![](media/image807.png) , ![](media/image808.png)  represents the angle at which the ray hits the surface of the window and ![](media/image809.png)  represents the exterior element azimuth and altitude relative to the window (see Figure 63).

![](media/image810.png)\


Figure 63: Exterior Luminance Element

This will produce luminous flux at the interior side of the window:

![](media/image811.png)\


where ![](media/image812.png)  is given by equation , ![](media/image813.png)  is the window transmittance at the incoming angle ![](media/image814.png)  and ![](media/image815.png)  is infinitesimal window element surface area.

Integrating luminous flux given by equation  over entire window area will give luminous flux on the interior side of the window.

To calculate the interan average reflected illumiance from the window, a simillar approch will be used as described in the chapter "Internally-Reflected Component of Interior Daylight Illuminance", which gives the flux balance equation:

![](media/image816.png)\


where ![](media/image817.png) represents first reflected flux which in the case of non-CFS is calculated by using the split-flux method.  In the case of CFS, first the reflected flux can be calculated more precisely by using light patches area.  Since each CFS is described by a BSDF, it is possible to calculate the light patches for each outgoing direction (see Figure 64).

![](media/image818.png)\


Figure 64: Complex Fenestration System Light Patches

Considering the total flux passing through the window at the outgoing direction "k", the first reflect flux (![](media/image819.png) ) can be calculated as:

![](media/image820.png)\


where ![](media/image821.png)  is the total flux entering the zone at the BSDF outgoing direction "k" and is calculated by integrating equation  over the entire window area for a given outgoing direction "k", N is the number of interior surfaces hit by the BSDF outgoing direction "k" (see Figure 64), ![](media/image822.png)  is the surface (patch) area which the window from the BSDF outgoing direction "k" makes at surface "a" and ![](media/image823.png)  is the visible reflectance from surface "a".

Summing all the outgoing directions will result in total first reflected flux:

![](media/image824.png)\


## Luminance from Exterior Elements

The case of CFS luminance from exterior elements can be calculated by using BSDF incoming directions defined for a given CFS.  Applying equation  for a certain incoming BSDF direction will give:

![](media/image825.png)\


This equation can be translated in the BSDF coordinate system where instead of standard azimuth and altitude angles, the equation will be transformed into the BSDF coordinate system.  In that case, equation  becomes:

![](media/image826.png)\


where ![](media/image827.png) represents the lambda value for the incoming direction "i".

## Luminous Flux and Direct Illuminance at Interior Side of the Window

In the case of CFS it is of interest to calculate fluxes for each outgoing BSDF direction.  For a given amount of illuminance at an incoming direction "i", flux into direction "o" will be calculated as:

![](media/image828.png)\


where ![](media/image829.png)  is the direct-direct transmittance for given incoming and outgoing directions ("i" and "o").

![](media/image830.png)\


Figure 65: Complex Fenestration System Flux Transition - Single Incoming Direction to Single Outgoing Direction

Total flux from the window at the outgoing direction "o" can be calculated by summing over all the incoming directions:

![](media/image831.png)\


where ![](media/image832.png)  is the number of BSDF incident directions.

Flux transmitted in the outgoing direction "o", calculated by equation , can be used for any given distribution of an exterior luminous element.  Knowing the intensity of the luminous flux, the direct illuminance at a certain point is calculated as:

![](media/image833.png)\


where ![](media/image834.png)  is the direct illuminance at the BSDF outgoing direction "o", ![](media/image835.png)  is the spatial angle of the element viewed from a certain reference point and ![](media/image836.png)  is the altitude of the window element viewed from the reference point.

## Handling Exterior Obstructions

Exterior obstructions are handled for two different cases.  The first case is to handle obstructions between the luminous element (sky/ground/sun) and the CFS window element. In that case, the direct illuminance at the window element is modified for the product of all the obstruction transmittances between the luminous and window elements, and equation  is modified as:

![](media/image837.png)\


where ![](media/image838.png)   is the number of exterior obstructions and ![](media/image839.png)  is the visible transmittance of obstruction "p".

The second case is to handle all obstructions between the luminous elements and the ground element.  The luminance from the ground element is calculated as:

![](media/image840.png)\


where ![](media/image841.png)  is the horizontal illuminance and ![](media/image842.png)  is the ground reflectance.  Horizontal illuminance is obtained by the integration over sky/sun elements and it is of interest to examine if all elements are visible from a certain ground point.  The integration examines if the ray between the sky and the ground element is hitting an obstruction.  It does not take into account whether or not the obstruction is transparent, it simply counts the number of hits.  The sky obstruction multiplier is calculated as:

![](media/image843.png)\


and in case the incoming element is from the ground, the illuminance at the window element (equation ) will be additionally modified from the sky obstruction multiplier:

![](media/image844.png)\
