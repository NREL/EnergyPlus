# Diffuse Reflection of Beam Solar and Sky Solar Radiation

A ray-tracing method is used to calculate beam solar and sky solar radiation that is diffusely reflected onto each of a building's exterior surfaces (walls, roofs, windows and doors), called here "receiving surfaces." The calculation begins by generating a set of rays proceeding into the outward hemisphere at each receiving point on a receiving surface. Then it determinines whether each ray hits the sky, ground or an obstruction. The radiance at the hit point from reflection of incident beam or sky solar is determined and the contribution of this radiance to the receiving surface is calculated, added to the contribution from other hit points, and averaged over the receiving points. Separate calculations are done for beam-to-diffuse and sky solar reflection from all obstructions and beam-to-diffuse and sky solar reflection from the ground. (For beam-to-beam reflection see "Beam Solar Radiation Specularly Reflected from Obstructions," below.)

## *Receiving points*

An *N*-sided surface is assigned *N* receiving points with the following coordinates, expressed in terms of the surface vertex coordinates:

![](media/image652.png)\


where

*P~ij~ = j^th^* coordinate of the *i^th^* receiving point

*v~kj~* = *j^th^* coordinate of the *k^th^* surface vertex

If *N* = 3:  *a~kj~*~~= 3/5 if *k* = *i*;  = 1/5 otherwise

If *N* > 3:  *a~kj~*~~= ![](media/image653.png)  if *k* = *i*;  = ![](media/image654.png)  otherwise

For example, for a vertical 3m by 5m rectangle with vertices (0,0,3), (0,0,0), (5,0,0) and (5,0,3), this expression gives receiving points at (1.25,0,2.25), (1.25,0,0.75), (3.75,0,0.75) and (3.75,0,2.25), as shown in Figure 53.

![Vertical rectangular exterior heat transfer surface showing location of receiving points for calculating incident solar radiation reflected from obstructions.](media/vertical-rectangular-exterior-heat-transfer.png)


## *Rays*

A total of 90 rays are sent out into the exterior hemisphere surrounding each receiving point. An upgoing ray may hit an obstruction or the sky. A downgoing ray may hit an obstruction or the ground. See Figure 54.

In subroutine InitSolReflRecSurf, the following is determined for each ray, *i,* :

Unit vector in direction of ray

Cosine of angle between ray and plane of receiving surface (![](media/image656.png) )

Element of solid angle associated with ray (![](media/image657.png) )

If the ray hits one or more obstructions, the coordinates of the hit point on the obstruction nearest the receiving point

For the surface containing the hit point: the surface number, the solar reflectance (![](media/image658.png) if an obstruction), and the surface unit vector at the hit point pointing into the hemisphere containing the receiving point

If the ray is downgoing and hits the ground, the coordinates of the ground hit point

Distance from receiving point to hit point

![Two-dimensional schematic showing rays going outward from a point on a receiving surface. Rays 1-6 hit the ground, rays 7-11 hit an obstruction, and rays 12-15 hit the sky.](media/two-dimensional-schematic-showing-rays-going.png)


## Sky Solar Radiation Diffusely Reflected from Obstructions

The factor for reflection of sky radiation from obstructions onto a receiving surface is calculated in subroutine CalcSkySolDiffuseReflFactors. It is given by:

![](media/image660.png)\


where

RecSurfNum is the receiving surface number,

![](media/image661.png) is the number of receiving points,

![](media/image662.png) is the number of rays,

*"obs,i*" denotes the obstruction hit by ray *i*, **

*Hit~obs,i~* = 1 if ray *i* hits an obstruction, = 0 otherwise,

*ViewFacSky~obs,i~* = unobstructed sky view factor of the obstruction = ![](media/image663.png) ,

*DifShdgRatioIsoSky~obs,i~* = (obstructed sky irradiance on obstruction)/(unobstructed sky irradiance on obstruction)

In this equation the product *ViewFacSky\*DifShdgRatioIsoSky* is the sky irradiance at the hit point divided by the horizontal sky irradiance taking into account shadowing of sky diffuse radiation on the obstruction by other obstructions, and assuming that the radiance of the sky is uniform. Note that we ignore secondary reflections here and in the following sections. In the present case this means that the irradiance at the hit point due to reflection of sky radiation from the ground or from other obstructions is not considered.

The above reflection factor is used in the timestep calculation to find the irradiance on a receiving surface due to sky radiation reflected from obstructions:

~~~~~~~~~~~~~~~~~~~~

    QRadSWOutIncSkyDiffReflObs(RecSurfNum) = DifSolarRad * ReflFacSkySolObs(RecSurfNum)  (W/m^2^)
~~~~~~~~~~~~~~~~~~~~

where *DifSolarRad* is the horizontal sky irradiance on an unobstructed horizontal plane (W/m^2^).

## Sky Solar Radiation Diffusely Reflected from the Ground

If a downgoing ray from a receiving point hits the ground (for example, rays 1-6 in Figure 54), the program calculates the radiance at the ground hit point due to sky diffuse solar reaching that point. To do this, rays are sent upward from the ground hit point and it is determined which of these rays are unobstructed and so go to the sky (for example, rays 6-10 in Figure 55). For this calculation it is assumed that the radiance of the sky is uniform.

![Two-dimensional schematic showing rays going upward from a ground hit point.](media/two-dimensional-schematic-showing-rays-going-001.png)


The factor for reflection of sky radiation from the ground onto a receiving surface is calculated in subroutine CalcSkySolDiffuseReflFactors. It is given by:

![](media/image665.png)\


where

*![](media/image666.png)*  ** denotes an upgoing ray from the ground point hit by ray *i* from the receiving point,

*Hit~sky,j(i)~* = 1 if ray *j(i*) hits the sky, = 0 otherwise,

![](media/image667.png) is the angle of incidence of ray ![](media/image666.png) with respect to the ground plane,

![](media/image668.png) is the solid angle element associated with ray ![](media/image666.png) .

This factor is used in the timestep calculation to find the irradiance on a receiving surface due to sky radiation reflected from the ground:

~~~~~~~~~~~~~~~~~~~~

    QRadSWOutIncSkyDiffReflGnd(RecSurfNum) = DifSolarRad * ![](media/image669.png) * ReflFacSkySolGnd(RecSurfNum)  (W/m^2^)
~~~~~~~~~~~~~~~~~~~~

where ![](media/image670.png) is the solar reflectance of the ground, which is assumed to be uniform over the ground plane but may vary monthly (because of snow cover, for example).

## Beam Solar Radiation Diffusely Reflected from Obstructions

This calculation is similar to that for sky solar reflected from obstructions. However, to find the radiance at a hit point on an obstruction a line is drawn from the hit point to center of the sun. From this line it is determined (1) if there is an obstruction between the hit point and the sun, in which case it is assumed that no beam solar reaches the hit point; and (2) if beam solar does reach the hit point, what the the solar angle of incidence at that point is.

The calculation is done for the hourly sun positions on each of the design days. It is also done for hourly sun positions on selected days during the weather file run period (the same days for which the shadowing calculations are done).

The factor for diffuse reflection of beam solar radiation from obstructions onto a receiving surface is calculated in subroutine CalcBeamSolDiffuseReflFactors. It is given by:

![](media/image671.png)\


where

 *IHr* = hour number

![](media/image672.png) = 1 if ray *i* from the receiving point hits an obstruction, = 0 otherwise,

![](media/image673.png) = 1 if the line from ray *i*'s hit point to the sun is unobstructed, = 0 otherwise,

![](media/image674.png)  is the angle of incidence of the sun on the obstruction.

This factor is used in the timestep calculation to find the diffuse irradiance on a receiving surface due to beam solar diffusely reflected from obstructions:

~~~~~~~~~~~~~~~~~~~~

    QRadSWOutIncBmToDiffReflObs(RecSurfNum) = BeamSolarRad *(WeightNow * ReflFacBmToDiffSolObs(RecSurfNum,HourOfDay) +WeightPreviousHour * ReflFacBmToDiffSolObs(RecSurfNum,PreviousHour))
~~~~~~~~~~~~~~~~~~~~

where *BeamSolarRad* is the timestep value of beam normal solar intensity (W/m2), and *WeightNow* and *WeightPreviousHour* are time-averaging factors.

## Beam Solar Radiation Diffusely Reflected from the Ground

This calculation is the same as that for beam solar diffusely reflected from obstructions except that only rays from a receiving point that hit the ground are considered. The factor for diffuse reflection of beam solar from the ground onto a receiving surface is calculated in subroutine CalcBeamSolDiffuseReflFactors. It is given by:

![](media/image675.png)\


where

![](media/image676.png) = 1 if ray *i* hits the ground, = 0 otherwise,

![](media/image677.png) = 1 if the line from ray *i*'s hit point ot the sun is unobstructed, = 0 otherwise,

![](media/image678.png) = angle of incidence of sun on ground (= solar zenith angle).

This factor is used in the timestep calculation to find the diffuse irradiance on a receiving surface due to beam solar diffusely reflected from the ground:

~~~~~~~~~~~~~~~~~~~~

    QRadSWOutIncBmToDiffReflGnd(RecSurfNum) = BeamSolarRad * ![](media/image679.png) *(WeightNow * ReflFacBmToDiffSolGnd(RecSurfNum,HourOfDay) +WeightPreviousHour * ReflFacBmToDiffSolGnd(RecSurfNum,PreviousHour))
~~~~~~~~~~~~~~~~~~~~

## Beam Solar Radiation Specularly Reflected from Obstructions

Figure 56 shows schematically how specular (beam-to-beam) reflection from an obstruction is calculated.

![Two-dimensional schematic showing specular reflection from an obstruction such as the glazed façade of a neighboring building. The receiving point receives specularly reflected beam solar radiation if (1) DB passes through specularly reflecting surface EF, (2) CD does not hit any obstructions (such as RS), and (3) AC does not hit any obstructions (such as PQ).](media/two-dimensional-schematic-showing-specular.png)


The calculation procedure is as follows:

Select receiving point D on receiving surface JK.

Select specularly reflecting surface EF.

Find the mirror image, B, of the sun with respect to the plane of EF and construct ray DB.

Check if DB passes through EF; if yes, find intersection point C and construct ray CD.

Check if CD is obstructed.

If no, construct ray AC and check if it is obstructed.

If no, find reflected beam irradiance (W/m^2^) at D:

![](media/image681.png)\


where

![](media/image682.png) = angle of incidence of beam solar at point C of the obstruction,

![](media/image683.png) = reflectance of obstruction as a function of the angle of incidence,

![](media/image684.png) = angle of incidence of ray CD on JK.

The factor for specular reflection of beam solar from obstruction onto a receiving surface is calculated in subroutine CalcBeamSolSpecularReflFactors. It is given by:

![](media/image685.png)\


The program assumes that specular reflection from a surface is due to glazing. If the reflecting surface is a window belonging to the building itself (as in Figure 51), then ![](media/image686.png) is the fraction of the window that is glazed (which is 1.0 unless the window has dividers).

If the surface is a shading surface (that represents, for example, the glazed façade of a neigboring building) the surface reflection information is entered with the Shading Surface Reflectance object. This object contains values for:

Diffuse solar reflectance of the unglazed part of the shading surface

Diffuse visible reflectance of the unglazed part of the shading surface

Fraction of shading surface that is glazed

Name of glazing construction

In this case![](media/image686.png) is "Fraction of shading surface that is glazed" and ![](media/image687.png) is the front reflectance of the indicated glazing construction as a function of beam solar incidence angle.

The above specular reflection factor is used in the timestep calculation to find the beam irradiance on a receiving surface due to beam-beam reflection from obstructions:

~~~~~~~~~~~~~~~~~~~~

    QRadSWOutIncBmToBmReflObsRecSurfNum) = BeamSolarRad * (WeightNow * ReflFacBmToBmSolObs(RecSurfNum,HourOfDay) +WeightPreviousHour * ReflFacBmToBmSolObs(RecSurfNum,PreviousHour))
~~~~~~~~~~~~~~~~~~~~