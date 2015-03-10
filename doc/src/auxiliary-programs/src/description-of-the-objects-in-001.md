# Description of the Objects in the BasementGHT.idd

These objects also appear in the main Energy+.IDD file with the prefix "GroundHeatTransfer:Basement:"

## SimParameters or GroundHeatTransfer:Basement:SimParameters Object

### Field:  F: Multiplier for the ADI solution:

This field specifies an alternating direction iteration scheme multiplier. It should normally be set to 0.1. It can be increased if the soil conductivity is high.

### Field: IYRS: Maximum number of yearly iterations: 

This specifies the maximum number of years the simulation will be allowed to run. If convergence is reached, the simulation will not run this long. It can be used to stop simulation before quasi steady convergence state is reached.

## MatlProps or GroundHeatTransfer:Basement:MatlProps Object

### Field: NMAT: Number of materials in this domain

This field specifies the number of materials whose properties are going to be specified. The order of the materials is not adjustable so six materials need to be specified. For the EnergyPlus basement application, only the foundation wall, floor slab, soil, and gravel are used.

### Field: Density for Foundation Wall

This field is the density (kg/m3) of the foundation wall. Typical value is 2243.

### Field: Density for Floor Slab

This field is the density (kg/m3) of the floor slab. Typical value is 2243.

### Field: Density for Ceiling

This field is the density (kg/m3) of the ceiling. Typical value is 311.

### Field: Density for Soil

This field is the density (kg/m3) of soil. Typical value is 1500.

### Field: Density for Gravel

This field is the density (kg/m3) of gravel. Typical value is 2000.

### Field: Density for Wood

This field is the density (kg/m3) of wood. Typical value is 449.

### Field: Specific Heat for Foundation Wall

This field is the specific heat (J/kg-K) of the foundation wall. Typical value is 880.

### Field: Specific Heat for Floor Slab

This field is the specific heat (J/kg-K) of the floor slab. Typical value is 880.

### Field: Specific Heat for Ceiling

This field is the specific heat (J/kg-K) of the ceiling. Typical value is 1530.

### Field: Specific Heat for Soil

This field is the specific heat (J/kg-K) of soil. Typical value is 840.

### Field: Specific Heat for Gravel

This field is the specific heat (J/kg-K) of gravel. Typical value is 720.

### Field: Specific Heat for Wood

This field is the specific heat (J/kg-K) of wood. Typical value is 1530.

### Field: Thermal Conductivity for Foundation Wall

This field is the thermal conductivity (W/m-K) of the foundation wall. Typical value is 1.4.

### Field: Thermal Conductivity for Floor Slab

This field is the thermal conductivity (W/m-K) of the floor slab. Typical value is 1.4.

### Field: Thermal Conductivity for Ceiling

This field is the thermal conductivity (W/m-K) of the ceiling. Typical value is 0.09.

### Field: Thermal Conductivity for Soil

This field is the thermal conductivity (W/m-K) of soil. Typical value is 1.1.

### Field: Thermal Conductivity for Gravel

This field is the thermal conductivity (W/m-K) of gravel. Typical value is 1.9.

### Field: Thermal Conductivity for Wood

This field is the thermal conductivity (W/m-K) of wood. Typical value is 0.12.

## Insulation or GroundHeatTransfer:Basement:Insulation Object

This object describes the insulation.

### Field: REXT: R Value of any exterior insulation

This field is the R value (m2-K/W) of any exterior insulation.

### Field: INSFULL: Flag: Is the wall fully insulated?

This field uses "true" for fully insulated walls and "false" for insulation half way down the side wall from the grade line.

## SurfaceProps or GroundHeatTransfer:Basement:SurfaceProps Object

The first six fields in this object specify the parameters used in the surface heat transfer boundary condition determination. They are specified for no snow and snow covered conditions.

### Field: ALBEDO: Surface albedo for No snow conditions

This field specifies the surface solar albedo for no snow conditions. Solar albedo is more commonly available than the solar absorptivity. The total solar energy absorbed by a horizontal surface is (1-albedo)\*(Total solar). Typical value for this field is 0.16.

### Field: ALBEDO: Surface albedo for snow conditions

This field specifies the surface solar albedo for snow covered conditions. Solar albedo is more commonly available than the solar absorptivity. The total solar energy absorbed by a horizontal surface is (1-albedo)\*(Total solar). Typical value for this field is 0.40.

### Field: EPSLN: Surface emissivity No Snow

This field specifies the long wavelength or thermal radiation emissivity for the ground surface under no snow conditions. Typical value is 0.94.

### Field: EPSLN: Surface emissivity with Snow

This field specifies the long wavelength or thermal radiation emissivity for the ground surface under snow covered conditions. Typical value is 0.86.

### Field: VEGHT: Surface roughness No snow conditions

This field specifies the surface roughness or vegetation height (cm) that is used in determining the convective heat transfer coefficient on the surface under no snow conditions. Typical value is 6.0.

### Field: VEGHT: Surface roughness snow conditions

This field specifies the surface roughness or vegetation height (cm) that is used in determining the convective heat transfer coefficient on the surface under snow covered conditions. Typical value is 0.25.

### Field: PET: Flag, Potential evapotranspiration on?

This field is a flag that invokes the evapotranspiration calculation at the surface. This covers all forms of latent heat transfer from the surface. It normally should be included. The user enters "true" for "yes" and "false" for no.

## BldgData or GroundHeatTransfer:Basement:BldgData Object

This object specifies the major configuration parameters for the basement. The 3-D grid used in the simulation has the capability of including a gravel "fill" around the basement. Thus several dimensions must be specified. All units are in meters.

### Field: DWALL: Wall thickness

This field specifies the basement wall thickness (m). Typical value is 0.2.

### Field: DSLAB: Floor Slab thickness

This field specifies the thickness of the floor slab (m). Typical value is 0.25.

### Field: DGRAVXY: Width of gravel pit beside basement wall

This field specifies the width of the gravel "fill" bed beside the basement wall (m).

### Field: DGRAVZN: Gravel depth extending above the floor slab

This field specifies the depth of the gravel fill above the floor slab (m).

### Field: DGRAVZP: Gravel depth below the floor slab

This field specifies the depth of the gravel fill below the floor slab (m).

## Interior or GroundHeatTransfer:Basement:Interior Object

This object provides the information needed to simulate the boundary conditions inside the basement.

### Field: COND: Flag: Is the basement conditioned?

This flag indicates that the basement temperature is controlled. For EnergyPlus runs, this should be TRUE.

### Field: HIN: Downward convection only heat transfer coefficient

This field specifies the convection only heat transfer coefficient for floors (downward heat flux – W/m2-K).

### Field: HIN: Upward convection only heat transfer coefficient

This field specifies the convection only heat transfer coefficient for floors (upward heat flux – W/m2-K).

### Field: HIN: Horizontal convection only heat transfer coefficient

This field specifies the convection only heat transfer coefficient for walls (horizontal heat flux – W/m2-K).

### Field: HIN: Downward combined (convection and radiation) heat transfer coefficient

This field specifies the combined thermal radiation and convection heat transfer coefficient for floors (downward heat flux – W/m2-K).

### Field: HIN: Upward combined (convection and radiation) heat transfer coefficient

This field specifies the combined thermal radiation and convection heat transfer coefficient for floors (upward heat flux – W/m2-K).

### Field: HIN: Horizontal combined (convection and radiation) heat transfer coefficient

This field specifies the combined thermal radiation and convection heat transfer coefficient for walls (horizontal heat flux – W/m2-K).

## ComBldg or GroundHeatTransfer:Basement:ComBldg Object

This object specifies the 12 monthly average basement temperatures (air temperature). The last field allows a daily sine wave variation to be placed on the inside temperature. During testing, it was found that entering values for the daily sine wave amplitude had very little effect on the results.

### Field: January average temperature

This field is the average air temperature (C) for the basement for January.

### Field: February average temperature

This field is the average air temperature (C) for the basement for February.

### Field: March average temperature

This field is the average air temperature (C) for the basement for March.

### Field: April average temperature

This field is the average air temperature (C) for the basement for April.

### Field: May average temperature

This field is the average air temperature (C) for the basement for May.

### Field: June average temperature

This field is the average air temperature (C) for the basement for June.

### Field: July average temperature

This field is the average air temperature (C) for the basement for July.

### Field: August average temperature

This field is the average air temperature (C) for the basement for August.

### Field: September average temperature

This field is the average air temperature (C) for the basement for September.

### Field: October average temperature

This field is the average air temperature (C) for the basement for October.

### Field: November average temperature

This field is the average air temperature (C) for the basement for November.

### Field: December average temperature

This field is the average air temperature (C) for the basement for December.

### Field: Daily variation sine wave amplitude 

This field is the amplitude (variation) for a daily sine wave variation to be placed on the inside temperature. This has been shown to have little effect on results so zero can be used safely.

## EquivSlab or GroundHeatTransfer:Basement:EquivSlab Object

This object provides the information needed to do the simulation as an equivalent square geometry by utilizing the area to perimeter ratio. This procedure was shown to be accurate by Cogil (1998).

### Field: APRatio: The area to perimeter ratio for this slab 

This field specifies the Area to Perimeter (A/P) ratio for the slab.

### Field: EquivSizing: Flag 

This field should always be TRUE unless the user wants to specifically look at the differences between the long wall and the short wall on a rectangular configuration.

## EquivAutoGrid or GroundHeatTransfer:Basement:EquivAutoGrid Object

This is a necessary object when the EquivSizing Flag in the EquivSlab object is TRUE. This object provides the information needed to set up the three dimensional conduction grid surrounding the basement.

### Field: CLEARANCE: Distance from outside of wall to edge of 3-D ground domain

This field specifies the distance to the outside of the field from the basement wall. It has been determined that 15 meters is quite satisfactory.

### Field: SlabDepth: Thickness of the floor slab

This field specifies the thickness of the slab in meters. Note that the slab top surface is level with the ground surface, so this is the depth into the ground. The slab depth has a significant effect on the temperature calculation, and it is also important for the auto-grid process. The finite difference grids are set in such a way that they use the slab thickness to determine the vertical grid spacing. Because of this, autogridding will fail if the slab thickness is specified larger than 0.25 meters. The program also is set up so that the slab is a single finite difference cell in the vertical direction. Thus, if the slab thickness is set too large, the accuracy of the calculation may be suspect. Typical value is 0.1.

### Field: BaseDepth: Depth of the basement wall below grade

This field specifies the depth of the basement wall below grade level (m). This is the height of the wall above the footing.

Field N2 specifies the height of the basement wall above the grade level. This should be zero for EnergyPlus runs since if an above grade segment is to be simulated, it should be done with EnergyPlus not with the basement program.

Field N3 specifies the floor slab thickness.

Field N4 specifies the depth of the basement wall below grade level.  This is the height of the wall above the footing.

## Sample idf File - Basement

~~~~~~~~~~~~~~~~~~~~
    ! Minneapolis Test Cases
    SimParameters,
     0.1,     ! [F: Multiplier for the ADI solution: 0<F<1.0,
              !         typically 0.1 (0.5 for high k
     1;       ! [IYRS: Maximum number of iterations: typically 25-30
    !
    MatlProps,
    6, ! [NMAT: Number of materials in this domain, UP TO 6]
    2242.6, 2242.6, 311.66, 1500.0, 2000.0, 448.5,![RHO(1-6): Matl density
    !(Fndn wall, Floor Slab, Ceiling,Soil, Gravel, Wood), kg/m3, ]
     880.0, 880.0, 1513.0, 840.0, 720.0, 1630.0,!       [CP(1-6): Specific Heat: J/kg-K,
     1.402, 1.402, 0.093, 0.5, 1.9, 0.119;  !       [TCON(1-6): Conductivity: W/m-K
    !
    Insulation,
     5.0, ! [REXT: R Value of any exterior insulation, K/(W/m2)]
    TRUE; ! [INSFULL: Flag: Is the wall fully insulated? TRUE/FALSE]
    !
    SurfaceProps,
    .16, .40, ! [ALBEDO: Surface albedo array, .16 .40]
    .94, .86, ! [EPSLN: Surface emissivity No Snow/ Snow .94 .86]
    6.0, 0.25, ! [VEGHT: Surface roughness NS/S, cm, 6.0, 0.25]
    TRUE;! [PET: Flag, Potential evapotranspiration on? T/F]
    !   Typically, PET is True
    !
    BldgData,
    .2, ! [DWALL: Wall thickness, m, .2]
    .1, ! [DSLAB: Floor slab thickness, m, 0.1]
    .3, ! [DGRAVXY: Width of gravel pit beside footing, m, 0.3]
    .2, ! [DGRAVZN: Gravel depth above the floor slab, m, 0.2]
     .1;! [DGRAVZP: Gravel depth below the floor slab, m, 0.1]
    !
    Interior,
     TRUE, ! [COND: Flag: Is the basement conditioned? TRUE/FALSE]
    ! [HIN: Indoor convective heat transfer coefficients, W/m2-K
    !       Convection Only: 1)Q Downward 2)Q Upward 3)Q Horizontal
    !       Conv and Radiation: 4)Q Downward 5)Q Upward 6)Q Horizontal]
    0.92, !Q Downward Convection Only
     4.04,!Q Upward Convection Only
     3.08,!Q HorizontalConvection Only
    6.13, !Q Downward Conv and Radiation
    9.26, !Q Upward Conv and Radiation
     8.29;!Q Horizontal Conv and Radiation
    !
    EquivSlab,
    15.0, ! [APRatio: The area to perimeter ratio for this slab: m]
    !
    TRUE;! [EquivSizing: Flag: Will the dimensions of an equivalent
    !       slab be calculated (TRUE) or will the dimensions be
    !              input directly? (FALSE)]
    !             Only advanced special simulations should use FALSE.
    !
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~
    EquivAutoGrid,   ! NOTE: EquivAutoGrid necessary when EquivSizing=TRUE
    !                This is the normal case.
    ! If the modelled building is not a rectangle or square, Equivalent
    ! sizing MUST be used to get accurate results
    !
    15, ! [CLEARANCE: Distance from outside of wall to edge, 15m]
    .1,  ! [SlabDepth: Thickness of the floor slab, m, 0.1]
    2.4; ! [BaseDepth: Depth of the basement wall below grade, m]
    !
    !
    ComBldg, !  Commercial building
    20.,  !  Jan Ave basement temp
    20.,  !  Feb Ave basement temp
    20.,  !  Mar Ave basement temp
    20.,  !  Apr Ave basement temp
    20.,  !  May Ave basement temp
    20.,  !  Jun Ave basement temp
    20.,  !  Jul Ave basement temp
    20.,  !  Aug Ave basement temp
    20.,  !  Sep Ave basement temp
    20.,  !  Oct Ave basement temp
    20.,  !  Nov Ave basement temp
    20.,  !  Dec Ave basement temp
    0.0;  !  Daily variation Sine Wave amplitude
~~~~~~~~~~~~~~~~~~~~

## Additional Objects

There are five additional objects in the IDD that can be used under very special situations by researchers who want to generate special calculation grids. They are normally not useful to EnergyPlus users. They will be shown as IDD sections only. They do not need to be in the IDF.

~~~~~~~~~~~~~~~~~~~~
    AutoGrid,
    \memo AutoGrid only necessary when EquivSizing is false
    \memo If the modelled building is not a rectangle or square, Equivalent
    \memo sizing MUST be used to get accurate results
           N1, \field CLEARANCE: Distance from outside of wall to edge, 15m]
           N2, \field SLABX: X dimension of the building slab, 0-60.0 m]
           N3, \field SLABY: Y dimension of the building slab, 0-60.0 m]
           N4, \field ConcAGHeight: Height of the fndn wall above grade, m]
           N5, \field SlabDepth: Thickness of the floor slab, m, 0.1]
           N6; \field BaseDepth: Depth of the basement wall below grade, m]
    !
    ManualGrid,
    \memo Manual Grid only necessary using manual gridding (not recommended)
           N1, \field NX: Number of cells in the X direction: 20]
           N2, \field NY: Number of cells in the Y direction: 20]
           N3, \field NZAG: Number of cells in the Z direction. above grade: 4 Always]
           N4, \field NZBG: Number of cells in Z direction. below grade: 10-35]
           N5, \field IBASE: X direction cell indicator of slab edge: 5-20]
           N6, \field JBASE: Y direction cell indicator of slab edge: 5-20]
           N7; \field KBASE: Z direction cell indicator of the top of the floor slab: 5-20]
    !
    XFACE,
    \memo This is only needed when using manual gridding (not recommended)
    \memo XFACE: X Direction cell face coordinates: m
           N1, N2, N3, N4, N5, N6, N7, N8, N9, N10, N11, N12, N13, N14,
           N15, N16, N17, N18, N19, N20, N21, N22, N23, N24, N25, N26,
           N27, N28, N29, N30, N31, N32, N33, N34, N35, N36, N37, N38,
           N39, N40, N41, N42, N43, N44;
    !

    YFACE,
    \memo This is only needed when using manual gridding (not recommended)
    \memo YFACE: Y Direction cell face coordinates: m
           N1, N2, N3, N4, N5, N6, N7, N8, N9, N10, N11, N12, N13, N14,
           N15, N16, N17, N18, N19, N20, N21, N22, N23, N24, N25, N26,
           N27, N28, N29, N30, N31, N32, N33, N34, N35, N36, N37, N38,
           N39, N40, N41, N42, N43, N44;
    !
    ZFACE,
    \memo This is only needed when using manual gridding (not recommended)
    \memo ZFACE: Z Direction cell face coordinates: m
           N1, N2, N3, N4, N5, N6, N7, N8, N9, N10, N11, N12, N13, N14,
           N15, N16, N17, N18, N19, N20, N21, N22, N23, N24, N25, N26,
           N27, N28, N29, N30, N31, N32, N33, N34, N35, N36, N37, N38,
           N39, N40;
~~~~~~~~~~~~~~~~~~~~