# Surface Heat Balance With Moveable Insulation

## Basic Heat Balance Cases

A heat balance must exist at the outside surface-air interface. The incoming conductive, convective, and radiative fluxes must sum up to zero:

![](media/image404.png)\


In contrast to the internal surface heat balance that treats all surfaces simultaneously, the external thermal balance for each surface is performed independent of all other surfaces. This implies that there is no direct interaction between the individual surfaces.

TARP includes four possible representations for the basic outside surface heat balance. The first two depend on which of the optimal surface conductance algorithms the user selects. The simple outside surface conductance that includes both the convective and thermal interchange between the surface and the environment in a single coefficient, is represented by the thermal network in Figure 29. Equation  can also be expressed as:

![](media/image405.png)\


This can be solved for the outside surface temperature.

![](media/image406.png)\


The detailed outside surface conductance model considers convection and radiant interchange with the sky and with the ground as separate factors. Its use in the outside thermal balance is shown in Figure 30. In this case, equation  can be expanded to give

![](media/image407.png)\


This can be solved for the outside surface temperature:

![](media/image408.png)\


The third and fourth representations occur when the outside surface has been covered with movable insulation. The insulation has a conductance of UM. The thermal network in Figure 31 represents this case. The insulation must be mass-less because it is not generally possible to perform a correct thermal balance at the juncture of two surfaces each modeled by CTF.

The equation for the thermal balance between the surface and the insulation is

![](media/image409.png)\


Which can be rewritten to solve for TO :

![](media/image410.png)\


Depending on whether or not the detailed or simple algorithm for surface conductance is being used, there are two expressions for TM, the outside temperature of the insulation. For the simple conductance:

![](media/image411.png)\


For the detailed conductance:

![](media/image412.png)\


In this case the values of HA, HS and HG must be found by using an estimated value of TM in place of TO.

![Thermal Network for Simple Outside Surface Coefficient](media/thermal-network-for-simple-outside-surface.png)


![Thermal Network for Detailed Outside Surface Coefficient](media/thermal-network-for-detailed-outside-surface.png)


![Thermal Network for Outside Moveable Insulation](media/thermal-network-for-outside-moveable.png)


## Heat Balance Cases

TO~t~and TI~t~are related through the Y~0~CTF~.~However TI~t~is also unknown. While it is possible to combine the outside and the inside surface heat balances to compute TO~t~and TI~t~simultaneously, TARP uses a simpler procedure where TO~t~is based on a previous value of TI. When Y~0~ is small, as occurs in well insulated or very massive surfaces, TI~t~can be replaced by TI~t -1~(which is known for the previous hour's heat balance) without significantly effecting the value of TO~t .~When Y~0~ is large, TO and TI can so strongly be coupled that separate outside and inside heat balances do not work because the environment and zone temperatures have negligible influence on the heat balances. The TARP uses the inside surface heat balance to couple TO~t~with TZ and TR. These two temperatures are less strongly influenced by TO and allow a reasonable heat balance. On the first heat balance iteration, TZ and TR are the values at time t-1. The user may optionally require that TO~t~be recomputed with every iteration of TI~t  .~ In this case TZ and TR have values from the previous iteration and a true simultaneous solution is achieved. In most conventional constructions, recomputing TO~t~does not significantly change the computed zone loads and temperatures. The inside surface heat balance is given by

![](media/image416.png)\


The surface heat balances can be combined in eight ways according to conditions for calculations of the outside surface temperature

![](media/image417.png)\


![](media/image418.png)\


![](media/image419.png)\


### Case1: Y~0~  small, simple conductance, no movable insulation:

From Equation

![](media/image420.png)\


### Case2: Y~0~ not small, simple conductance, no movable insulation:

From Equations  and

![](media/image421.png)\


### Case3: Y~0~  small, detailed conductance, no movable insulation:

From Equation

![](media/image422.png)\


### Case4: Y~0~ not small, detailed conductance, no movable insulation:

From Equations  and

![](media/image423.png)\


### Case5: Y~0~  small, simple conductance, with movable insulation:

From Equations  and

 ![](media/image424.png)

### Case6: Y~0~ not small, simple conductance, with movable insulation:

From Equations ,  and

![](media/image425.png)\


### Case7: Y~0~  small, detailed conductance, with movable insulation:

From Equations  and

![](media/image426.png)\


### Case8: Y~0~ not small, detailed conductance, with movable insulation:

From Equations ,  and

![](media/image427.png)\


## Fortran Algorithm Examples

### Case5: Y~0~  small, simple conductance, with movable insulation:

**From Equation**

~~~~~~~~~~~~~~~~~~~~

    ! Outside heat balance case: Movable insulation, slow conduction, simple convection
     F2 = DBLE(HmovInsul) / ( DBLE(HmovInsul) + DBLE(HExtSurf(SurfNum)) )
     TH(SurfNum,1,1) = (-CTFConstOutPart(SurfNum)  &
                        +DBLE(QRadSWOutAbs(SurfNum) )  &
               +Construct(ConstrNum)%CTFCross(0)*TempSurfIn(SurfNum)  &
               +F2* ( DBLE(QRadSWOutMvIns(SurfNum))                   &
               + DBLE(HExtSurf(SurfNum))* DBLE(TempExt) )   )         &
               /( Construct(ConstrNum)%CTFOutside(0) + DBLE(HmovInsul) &
- F2* DBLE(HMovInsul))
~~~~~~~~~~~~~~~~~~~~

### Case6: Y~0~ not small, simple conductance, with movable insulation:

From Equation

~~~~~~~~~~~~~~~~~~~~

    ! Outside heat balance case: Movable insulation, quick conduction, simple convection
    F2 = DBLE(HmovInsul) / ( DBLE(HmovInsul) + DBLE(HExtSurf(SurfNum)) )
    TH(SurfNum,1,1) = (-CTFConstOutPart(SurfNum)             &
    + DBLE(QRadSWOutAbs(SurfNum))                            &
    +F2*( DBLE(QRadSWOutMvIns(SurfNum))                      &
    +DBLE(HExtSurf(SurfNum))* DBLE(TempExt)  )               &
    +F1*( CTFConstInPart(SurfNum)                            &
    + DBLE(QRadSWInAbs(SurfNum))                             &
    + DBLE(QRadThermInAbs(SurfNum))                          &
    + DBLE(HConvIn(SurfNum))*MAT(ZoneNum)                    &
    + DBLE(NetLWRadToSurf(SurfNum))  ) )                     &
    /( Construct(ConstrNum)%CTFOutside(0) + DBLE(HmovInsul)  &
    -F2* DBLE(HMovInsul )- F1*Construct(ConstrNum)%CTFCross(0) )
~~~~~~~~~~~~~~~~~~~~

### Case7: Y~0~  small, detailed conductance, with movable insulation:

From Equation

~~~~~~~~~~~~~~~~~~~~

    ! Outside heat balance case: Movable insulation, slow conduction, detailed convection
    F2 = DBLE(HMovInsul)/ ( DBLE(HMovInsul) + DBLE(HExtSurf(SurfNum)) &
               +DBLE(HSky) + DBLE(HGround) )
    TH(SurfNum,1,1) = (-CTFConstOutPart(SurfNum)                      &
    +DBLE(QRadSWOutAbs(SurfNum))                                      &
    +Construct(ConstrNum)%CTFCross(0)*TempSurfIn(SurfNum)             &
    +F2*( DBLE(QRadSWOutMvIns(SurfNum))                               &
    +DBLE(HExtSurf(SurfNum))*DBLE(TempExt)                            &
    +DBLE(HSky)*DBLE(SkyTemp)                                         &
    +DBLE(HGround)*DBLE(OutDryBulbTemp) ) )                           &
    /( Construct(ConstrNum)%CTFOutside(0)                             &
    +DBLE(HMovInsul) - F2*DBLE(HMovInsul) )
~~~~~~~~~~~~~~~~~~~~

### Case8: Y~0~ not small, detailed conductance, with movable insulation:

From Equation

~~~~~~~~~~~~~~~~~~~~

    ! Outside heat balance case: Movable insulation, quick conduction, detailed convection
    F2 = DBLE(HMovInsul)/ ( DBLE(HMovInsul) + DBLE(HExtSurf(SurfNum)) &
               +DBLE(HSky) + DBLE(HGround) )
    TH(SurfNum,1,1) = (-CTFConstOutPart(SurfNum)                      &
    +DBLE(QRadSWOutAbs(SurfNum))                                      &
    +F1*( CTFConstInPart(SurfNum)                                     &
    +DBLE(QRadSWInAbs(SurfNum))                                       &
    +DBLE(QRadThermInAbs(SurfNum))                                    &
    +DBLE(HConvIn(SurfNum))*MAT(ZoneNum)                              &
    +DBLE(NetLWRadToSurf(SurfNum)) )                                  &
    +F2*( DBLE(QRadSWOutMvIns(SurfNum))                               &
    +DBLE(HExtSurf(SurfNum))*DBLE(TempExt)                            &
    +DBLE(HSky)*DBLE(SkyTemp)                                         &
    +DBLE(HGround)*DBLE(OutDryBulbTemp) )                             &
    /( Construct(ConstrNum)%CTFOutside(0)                             &
    +DBLE(HMovInsul) - F2*DBLE(HMovInsul)                             &
    -F1*Construct(ConstrNum)%CTFCross(0)   )
~~~~~~~~~~~~~~~~~~~~

## Fortran Variable Descriptions

Table: Fortran Variables and Descriptions

**FORTRAN Variable**|**Description**|**Tarp Variable**|**Units**|**Description**
---------------------------------|----------------------------|------------------------------|----------------------|----------------------------
TH(SurfNum,1,1)|Temperature History(SurfNum,Hist Term,In/Out), where: Hist Term (1 = Current Time, 2-MaxCTFTerms = previous times), In/Out (1 = Outside, 2 = Inside)|TO~t~|C|Temperature of outside of surface I at time t
Construct(ConstrNum)%CTFCross(0)|Cross or Y term of the CTF equation|Y0|W/m^2^K|Cross CTF term
Construct(ConstrNum)%CTFInside(0)|Inside or Z terms of the CTF equation|Z0|W/m^2^K|Inside CTF term
Construct(ConstrNum)%CTFOutside(0)|Outside or X terms of the CTF equation|X0|W/m^2^K|Outside CTF term
CTFConstInPart(SurfNum)|Constant inside portion of the CTF calculation|KIP~t~|W/m^2^|Portion of inward conductive flux based on previous temperature and flux history terms
CTFConstOutPart(SurfNum)|Constant Outside portion of the CTF calculation|KOP~t~|W/m^2^|Portion of outward conductive flux based on previous temperature and flux history terms
F1, F2, F3|Intermediate calculation variables|F1, F2, F3||Radiation interchange factor between surfaces
GroundTemp|Ground surface temperature|T~g~|C|Temperature of ground at the surface exposed to the outside environment
HConvIn(SurfNum)|Inside convection coefficient|HI|W/m^2^K|Inside convection coefficient
HExtSurf(SurfNum)|Outside Convection Coefficient|HO, HA|W/m^2^K|Overall outside surface conductance
HGround|Radiant exchange (linearized) coefficient|HG|W/m^2^K|Radiative conductance (outside surface to ground temperature
HmovInsul|Conductance or "h" value of movable insulation|UM|W/m^2^K|Conductance of Movable insulation
HSky|Radiant exchange (linearized) coefficient|HS|W/m^2^K|Radiative conductance (outside surface to sky radiant temperature
MAT(ZoneNum)|Zone temperature|TZ|C|Temperature of zone air
NetLWRadToSurf(SurfNum)|Net interior longwave radiation to a surface from other surfaces|HR\*TR|W/m^2^|Net surface to surface radiant exchange
QRadSWInAbs(SurfNum)|Short-wave radiation absorbed on inside of opaque surface|QSI|W/m^2^|Short wave radiant flux absorbed at inside of surface
QRadSWOutAbs(SurfNum)|Short wave radiation absorbed on outside opaque surface|QSO|W/m^2^|Short wave radiant flux absorbed at outside of surface
QRadSWOutMvIns(SurfNum)|Short wave radiation absorbed on outside of movable insulation|QSM|W/m^2^|Short wave radiant flux absorbed at surface of movable insulation
QRadThermInAbs(SurfNum)|Thermal Radiation absorbed on inside surfaces||W/m^2^|Longwave radiant flux from internal gains
SkyTemp|Sky temperature|T~s~|C|Sky temp
TempExt|Exterior surface temperature or exterior air temperature|TM, T~a~|C|Temperature of external surface of movable insulation or outside ambient air temperature
TempSurfIn(SurfNum)|Temperature of inside surface for each heat transfer surface|TI~t-1~|C|Temperature of inside of surface I at time t-1

## References

Walton, G.N. 1983. "The Thermal Analysis Research Program Reference Manual Program (TARP)", National Bureau of Standards (now National Institute of Standards and Technology).