Solar Radiation Reflected from Exterior Surfaces
================================================

EnergyPlus has an option to calculate beam and sky solar radiation that is reflected from exterior surfaces and then strikes the building. This calculation occurs if “withReflections” is used on the SolarDistribution option in the Building object. For zones with detailed daylighting, these reflections are also considered in the daylight illuminance calculations.[[6]](#_ftn6)

The reflecting surfaces fall into three categories:

* **Shadowing surfaces**. These are surfaces like overhangs or neighboring buildings entered with objects Shading:Site:Detailed, Shading:Building:Detailed, or Shading:Zone:Detailed Examples are shown in Figure 50.

These surfaces can have diffuse and/or specular (beam-to-beam)  reflectance values that are specified with the ShadingProperty:Reflectance object.

* **Exterior building surfaces**. In this case one section of the building reflects solar radiation onto another section (and vice-versa). See Figure 51.

Opaque building surfaces (walls, for example) are assumed to be diffusely reflecting. Windows and glass doors are assumed to be specularly reflecting. The reflectance values for opaque surfaces are calculated by the program from the Solar Absorptance and Visible Absorptance values of the outer material layer of the surface’s construction. The reflectance values for windows and glass doors are calculated by the program from the reflectance properties of the individual glass layers that make up surface’s construction assuming no shading device is present and taking into account inter-reflections among the layers.

* **The ground surface**. Beam solar and sky solar reflection from the ground is calculated even if “withReflections” is not used (the default). But in this case the ground plane is considered unobstructed, i.e., the shadowing of the ground by the building itself or by obstructions such as neighboring buildings is ignored. This shadowing is taken into account only if “WithReflections” is used in the Solar Distribution field (in “Building” input object) (Figure 52). In this case the user-input value of ground view factor is not used.

![](media/image670.png)

Figure 50.   Examples of solar reflection from shadowing surfaces in the Shading series of input objects. Solid arrows are beam solar radiation; dashed arrows are diffuse solar radiation. (a) Diffuse reflection of beam solar radiation from the top of an overhang. (b) Diffuse reflection of sky solar radiation from the top of an overhang. (c) Beam-to-beam (specular) reflection from the façade of an adjacent highly-glazed building represented by a vertical shadowing surface.

![](media/image671.png)

Figure 51.   Solar reflection from building surfaces onto other building surfaces. In this example beam solar reflects from a vertical section of the building onto a roof section. The reflection from the window is specular. The reflection from the wall is diffuse.

![](media/image672.png)

Figure 52. Shadowing by the building itself affects beam solar reflection from the ground.  Beam-to-diffuse reflection from the ground onto the building occurs only for sunlit areas, A and C, not for shaded area, B. Shadowing by the building also affects sky solar reflection from ground (not shown).

Diffuse Reflection of Beam Solar and Sky Solar Radiation
--------------------------------------------------------

A ray-tracing method is used to calculate beam solar and sky solar radiation that is diffusely reflected onto each of a building’s exterior surfaces (walls, roofs, windows and doors), called here “receiving surfaces.” The calculation begins by generating a set of rays proceeding into the outward hemisphere at each receiving point on a receiving surface. Then it determinines whether each ray hits the sky, ground or an obstruction. The radiance at the hit point from reflection of incident beam or sky solar is determined and the contribution of this radiance to the receiving surface is calculated, added to the contribution from other hit points, and averaged over the receiving points. Separate calculations are done for beam-to-diffuse and sky solar reflection from all obstructions and beam-to-diffuse and sky solar reflection from the ground. (For beam-to-beam reflection see “Beam Solar Radiation Specularly Reflected from Obstructions,” below.)

### *Receiving points*

An *N*-sided surface is assigned *N* receiving points with the following coordinates, expressed in terms of the surface vertex coordinates:

<div>$${P_{ij}} = \sum\limits_{k = 1}^N {{a_{ki}}{v_{kj}},{\rm{  }}i = 1,3;{\rm{  }}j = 1,3} $$</div>

where

*P<sub>ij</sub> = j<sup>th</sup>* coordinate of the* i<sup>th</sup>* receiving point

*v<sub>kj</sub>* =* j<sup>th</sup>* coordinate of the *k<sup>th</sup>* surface vertex

If *N* = 3:  *a<sub>kj</sub>* = 3/5 if *k*= *i*;  = 1/5 otherwise

If *N* &gt; 3:  *a<sub>kj</sub>* = <span>\(\frac{{N + 1}}{{2N}}\)</span> if *k*= *i*;  = <span>\(\frac{1}{{2N}}\)</span> otherwise

For example, for a vertical 3m by 5m rectangle with vertices (0,0,3), (0,0,0), (5,0,0) and (5,0,3), this expression gives receiving points at (1.25,0,2.25), (1.25,0,0.75), (3.75,0,0.75) and (3.75,0,2.25), as shown in Figure 53.



![](media/image676.png)

Figure 53. Vertical rectangular exterior heat transfer surface showing location of receiving points for calculating incident solar radiation reflected from obstructions.

### *Rays*

A total of 90 rays are sent out into the exterior hemisphere surrounding each receiving point. An upgoing ray may hit an obstruction or the sky. A downgoing ray may hit an obstruction or the ground. See Figure 54.

In subroutine InitSolReflRecSurf, the following is determined for each ray, *i,*:

1.    Unit vector in direction of ray

2.    Cosine of angle between ray and plane of receiving surface (<span>\(\cos {\alpha_i}\)</span>)

3.    Element of solid angle associated with ray (<span>\(d{\Omega_i}\)</span>)

4.    If the ray hits one or more obstructions, the coordinates of the hit point on the obstruction nearest the receiving point

5.    For the surface containing the hit point: the surface number, the solar reflectance (<span>\({\rho_{obs,i}}\)</span>if an obstruction), and the surface unit vector at the hit point pointing into the hemisphere containing the receiving point

6.    If the ray is downgoing and hits the ground, the coordinates of the ground hit point

7.    Distance from receiving point to hit point



![](media/image680.png)

Figure 54.  Two-dimensional schematic showing rays going outward from a point on a receiving surface. Rays 1-6 hit the ground, rays 7-11 hit an obstruction, and rays 12-15 hit the sky.

### Sky Solar Radiation Diffusely Reflected from Obstructions

The factor for reflection of sky radiation from obstructions onto a receiving surface is calculated in subroutine CalcSkySolDiffuseReflFactors. It is given by:

<div>$$\begin{array}{l}{\rm{ReflFacSkySolObs(RecSurfNum)}} = \\\frac{1}{{{N_{rec}}}}\sum\limits_1^{{N_{rec}}} {\sum\limits_{i = 1}^{{N_{ray}}} {Hi{t_{obs,i}}ViewFacSk{y_{obs,i}}DifShdgRatioIsoSk{y_{obs,i}}{\rho_{obs,i}}\cos {\alpha_i}/\pi } } \end{array}$$</div>

where

RecSurfNum is the receiving surface number,

<span>\({N_{rec}}\)</span>is the number of receiving points,

<span>\({N_{ray}}\)</span>is the number of rays,

“*obs,i*” denotes the obstruction hit by ray *i*,

*Hit<sub>obs,i</sub>* = 1 if ray* i* hits an obstruction, = 0 otherwise,

*ViewFacSky<sub>obs,i</sub>*= unobstructed sky view factor of the obstruction = <span>\((1 + \cos til{t_{obs}})/2\)</span>,

*DifShdgRatioIsoSky<sub>obs,i</sub>*= (obstructed sky irradiance on obstruction)/(unobstructed sky irradiance on obstruction)

In this equation the product*ViewFacSky\*DifShdgRatioIsoSky*is the sky irradiance at the hit point divided by the horizontal sky irradiance taking into account shadowing of sky diffuse radiation on the obstruction by other obstructions, and assuming that the radiance of the sky is uniform. Note that we ignore secondary reflections here and in the following sections. In the present case this means that the irradiance at the hit point due to reflection of sky radiation from the ground or from other obstructions is not considered.

The above reflection factor is used in the timestep calculation to find the irradiance on a receiving surface due to sky radiation reflected from obstructions:

QRadSWOutIncSkyDiffReflObs(RecSurfNum) =
 DifSolarRad \* ReflFacSkySolObs(RecSurfNum)  (W/m<sup>2</sup>)

where*DifSolarRad* is the horizontal sky irradiance on an unobstructed horizontal plane (W/m<sup>2</sup>).

### Sky Solar Radiation Diffusely Reflected from the Ground

If a downgoing ray from a receiving point hits the ground (for example, rays 1-6 in Figure 54), the program calculates the radiance at the ground hit point due to sky diffuse solar reaching that point. To do this, rays are sent upward from the ground hit point and it is determined which of these rays are unobstructed and so go to the sky (for example, rays 6-10 in Figure 55). For this calculation it is assumed that the radiance of the sky is uniform.



![](media/image685.png)

Figure 55.  Two-dimensional schematic showing rays going upward from a ground hit point.

The factor for reflection of sky radiation from the ground onto a receiving surface is calculated in subroutine CalcSkySolDiffuseReflFactors. It is given by:

<div>$$\begin{array}{l}{\rm{ReflFacSkySolGnd(RecSurfNum)}} = \\\frac{1}{{{N_{rec}}}}\sum\limits_1^{{N_{rec}}} {\sum\limits_{i = 1}^{{N_{ray}}} {\left[ {(Hi{t_{gnd,i}}d{\Omega_i}\cos {\alpha_i}/\pi )\sum\limits_{j(i)}^{{N_{ray}}} {Hi{t_{sky,j(i)}}\cos {\alpha_{j(i)}}d{\Omega_{j(i)}}/\pi } } \right]} } \end{array}$$</div>



where

*<span>\(j(i)\)</span> * denotes an upgoing ray from the ground point hit by ray *i*from the receiving point,

*Hit<sub>sky,j(i)</sub>* = 1 if ray* j(i*) hits the sky, = 0 otherwise,

<span>\({\alpha_{j(i)}}\)</span>is the angle of incidence of ray <span>\(j(i)\)</span>with respect to the ground plane,

<span>\(d{\Omega_{j(i)}}\)</span>is the solid angle element associated with ray <span>\(j(i)\)</span>.

This factor is used in the timestep calculation to find the irradiance on a receiving surface due to sky radiation reflected from the ground:

QRadSWOutIncSkyDiffReflGnd(RecSurfNum) =
 DifSolarRad \* <span>\({\rho_{gnd}}\)</span>\* ReflFacSkySolGnd(RecSurfNum)  (W/m<sup>2</sup>)

where <span>\({\rho_{gnd}}\)</span>is the solar reflectance of the ground, which is assumed to be uniform over the ground plane but may vary monthly (because of snow cover, for example).

### Beam Solar Radiation Diffusely Reflected from Obstructions

This calculation is similar to that for sky solar reflected from obstructions. However, to find the radiance at a hit point on an obstruction a line is drawn from the hit point to center of the sun. From this line it is determined (1) if there is an obstruction between the hit point and the sun, in which case it is assumed that no beam solar reaches the hit point; and (2) if beam solar does reach the hit point, what the the solar angle of incidence at that point is.

The calculation is done for the hourly sun positions on each of the design days. It is also done for hourly sun positions on selected days during the weather file run period (the same days for which the shadowing calculations are done).

The factor for diffuse reflection of beam solar radiation from obstructions onto a receiving surface is calculated in subroutine CalcBeamSolDiffuseReflFactors. It is given by:

<div>$${\rm{ReflFacBmToDiffSolObs(RecSurfNum,IHr)}} = \frac{1}{{{N_{rec}}}}\sum\limits_1^{{N_{rec}}} {\sum\limits_{i = 1}^{{N_{ray}}} {Hi{t_{obs,i}}Hi{t_{obs,i,sun}}d{\Omega_i}\cos {\alpha_i}{\rho_{obs,i}}\cos {\alpha_{sun,obs,i}}} } $$</div>

where

* IHr* = hour number

<span>\(Hi{t_{obs,i}}\)</span>= 1 if ray *i* from the receiving point hits an obstruction, = 0 otherwise,

<span>\(Hi{t_{obs,i,sun}}\)</span>= 1 if the line from ray *i*’s hit point to the sun is unobstructed, = 0 otherwise,

<span>\({\alpha_{sun,obs,i}}\)</span> is the angle of incidence of the sun on the obstruction.

This factor is used in the timestep calculation to find the diffuse irradiance on a receiving surface due to beam solar diffusely reflected from obstructions:

QRadSWOutIncBmToDiffReflObs(RecSurfNum) = BeamSolarRad \*
 (WeightNow \* ReflFacBmToDiffSolObs(RecSurfNum,HourOfDay) +
 WeightPreviousHour \* ReflFacBmToDiffSolObs(RecSurfNum,PreviousHour))


where *BeamSolarRad* is the timestep value of beam normal solar intensity (W/m2), and *WeightNow* and *WeightPreviousHour* are time-averaging factors.

### Beam Solar Radiation Diffusely Reflected from the Ground

This calculation is the same as that for beam solar diffusely reflected from obstructions except that only rays from a receiving point that hit the ground are considered. The factor for diffuse reflection of beam solar from the ground onto a receiving surface is calculated in subroutine CalcBeamSolDiffuseReflFactors. It is given by:

<div>$${\rm{ReflFacBmToDiffSolGnd(RecSurfNum,IHr)}} = \frac{1}{{{N_{rec}}}}\sum\limits_1^{{N_{rec}}} {\sum\limits_{i = 1}^{{N_{ray}}} {Hi{t_{gnd,i}}Hi{t_{gnd,i,sun}}d{\Omega_i}\cos {\alpha_{gnd,i}}\cos {\alpha_{sun,gnd}}} } $$</div>

where

<span>\(Hi{t_{gnd,i}}\)</span>= 1 if ray *i* hits the ground, = 0 otherwise,

<span>\(Hi{t_{gnd,i,sun}}\)</span>= 1 if the line from ray *i*’s hit point ot the sun is unobstructed, = 0 otherwise,

<span>\({\alpha_{sun,gnd}}\)</span>= angle of incidence of sun on ground (= solar zenith angle).

This factor is used in the timestep calculation to find the diffuse irradiance on a receiving surface due to beam solar diffusely reflected from the ground:

QRadSWOutIncBmToDiffReflGnd(RecSurfNum) = BeamSolarRad \* <span>\({\rho_{gnd}}\)</span>\*
 (WeightNow \* ReflFacBmToDiffSolGnd(RecSurfNum,HourOfDay) +
 WeightPreviousHour \* ReflFacBmToDiffSolGnd(RecSurfNum,PreviousHour))



### Beam Solar Radiation Specularly Reflected from Obstructions

Figure 56 shows schematically how specular (beam-to-beam) reflection from an obstruction is calculated. [[7]](#_ftn7)

![](media/image703.png)

Figure 56.  Two-dimensional schematic showing specular reflection from an obstruction such as the glazed façade of a neighboring building. The receiving point receives specularly reflected beam solar radiation if (1) DB passes through specularly reflecting surface EF, (2) CD does not hit any obstructions (such as RS), and (3) AC does not hit any obstructions (such as PQ).

The calculation procedure is as follows:

1.    Select receiving point D on receiving surface JK.

2.    Select specularly reflecting surface EF.

3.    Find the mirror image, B, of the sun with respect to the plane of EF and construct ray DB.

4.    Check if DB passes through EF; if yes, find intersection point C and construct ray CD.

5.    Check if CD is obstructed.

6.    If no, construct ray AC and check if it is obstructed.

7.    If no, find reflected beam irradiance (W/m<sup>2</sup>) at D:

<div>$$I_{D,refl}^{bm} = BeamSolarRad*{\rho_{spec}}({\alpha_C})\cos {\alpha_D}$$</div>

where

<span>\({\alpha_C}\)</span>= angle of incidence of beam solar at point C of the obstruction,

<span>\({\rho_{spec}}({\alpha_C})\)</span>= reflectance of obstruction as a function of the angle of incidence,

<span>\({\alpha_D}\)</span>= angle of incidence of ray CD on JK.

The factor for specular reflection of beam solar from obstruction onto a receiving surface is calculated in subroutine CalcBeamSolSpecularReflFactors. It is given by:

<div>$${\rm{ReflFacBmToBmSolObs(RecSurfNum,IHr) }} = \sum\limits_{\scriptstyle specularly\atop{\scriptstyle reflecting\atop\scriptstyle surfaces}} {} \left[ {\frac{1}{{{N_{rec}}}}\sum\limits_1^{{N_{rec}}} {{f_{C,glazed}}{\rho_{spec}}({\alpha_C})\cos {\alpha_D}} } \right]$$</div>

The program assumes that specular reflection from a surface is due to glazing. If the reflecting surface is a window belonging to the building itself (as in Figure 51), then <span>\({f_{C,glazed}}\)</span>is the fraction of the window that is glazed (which is 1.0 unless the window has dividers).

If the surface is a shading surface (that represents, for example, the glazed façade of a neigboring building) the surface reflection information is entered with the Shading Surface Reflectance object. This object contains values for:

1.    Diffuse solar reflectance of the unglazed part of the shading surface

2.    Diffuse visible reflectance of the unglazed part of the shading surface

3.    Fraction of shading surface that is glazed

4.    Name of glazing construction

In this case <span>\({f_{C,glazed}}\)</span> is “Fraction of shading surface that is glazed” and <span>\({\rho_{spec}}({\alpha_C})\)</span>is the front reflectance of the indicated glazing construction as a function of beam solar incidence angle.

The above specular reflection factor is used in the timestep calculation to find the beam irradiance on a receiving surface due to beam-beam reflection from obstructions:

QRadSWOutIncBmToBmReflObsRecSurfNum) = BeamSolarRad \*
 (WeightNow \* ReflFacBmToBmSolObs(RecSurfNum,HourOfDay) +
 WeightPreviousHour \* ReflFacBmToBmSolObs(RecSurfNum,PreviousHour))


